mod error;
mod place;
mod post;

use crate::db::DbPool;
use crate::error::{Error, HttpError};
use crate::redis::{RedisConn, RedisPool};
use crate::rmqpub::RmQPublisher;
use crate::{db, redis, rmqpub};

use diesel::PgConnection;
use domain::models::Agent;
use rocket::fairing::{Fairing, Info, Kind};
use rocket::http::{ContentType, Header};
use rocket::request::Request;
use rocket::response::Responder;
use rocket::serde::json::Json;
use rocket::{Build, Response, Rocket};
use serde::Serialize;

#[get("/health")]
pub fn health() -> String {
    "I'm awake !".to_string()
}

/// Catches all OPTION requests in order to get the CORS related Fairing triggered.
#[options("/<_..>")]
pub fn all_options() {
    /* Intentionally left empty */
}

pub struct Cors;

#[rocket::async_trait]
impl Fairing for Cors {
    fn info(&self) -> Info {
        Info {
            name: "Cross-Origin-Resource-Sharing Fairing",
            kind: Kind::Response,
        }
    }

    async fn on_response<'r>(&self, _request: &'r Request<'_>, response: &mut Response<'r>) {
        response.set_header(Header::new("Access-Control-Allow-Origin", "*"));
        response.set_header(Header::new(
            "Access-Control-Allow-Methods",
            "POST, PATCH, PUT, DELETE, HEAD, OPTIONS, GET",
        ));
        response.set_header(Header::new("Access-Control-Allow-Headers", "*"));
        response.set_header(Header::new("Access-Control-Allow-Credentials", "true"));
    }
}

pub struct EtagJson<T>
where
    T: Serialize,
{
    body: Json<T>,
    etag: Option<String>,
}
impl<T: Serialize> EtagJson<T> {
    pub fn new(body: T, etag: Option<String>) -> Self {
        Self {
            body: Json(body),
            etag: etag,
        }
    }
}

#[rocket::async_trait]
impl<'r, T: Serialize> Responder<'r, 'static> for EtagJson<T> {
    fn respond_to(self, req: &'r Request<'_>) -> rocket::response::Result<'static> {
        let mut builder = rocket::response::Response::build_from(self.body.respond_to(req)?);
        let builder = builder
            .status(rocket::http::Status::Ok)
            .header(ContentType::JSON);
        let builder = match self.etag {
            Some(tag) => builder.header(Header {
                name: "ETag".to_string().into(),
                value: tag.into(),
            }),
            None => builder,
        };

        builder.ok()
    }
}
use rocket::{
    http::Status,
    request::{FromRequest, Outcome},
};
pub struct IfNoneMatchHeader {
    pub etag: String,
}

fn extract_if_none_match(req: &Request<'_>) -> Result<IfNoneMatchHeader, Error> {
    use super::error::HttpError;
    let etag = req
        .headers()
        .get_one("If-None-Match")
        .ok_or(Error::HttpError(HttpError::NoIfNoneMatchHeader))?;

    Ok(IfNoneMatchHeader {
        etag: etag.to_string(),
    })
}

#[rocket::async_trait]
impl<'r> FromRequest<'r> for IfNoneMatchHeader {
    type Error = super::error::Error;

    async fn from_request(req: &'r Request<'_>) -> Outcome<Self, Self::Error> {
        let result = extract_if_none_match(req);

        match result {
            Ok(e) => Outcome::Success(e),
            Err(e) => Outcome::Error((Status::Unauthorized, e)),
        }
    }
}
pub struct IfMatchHeader {
    pub etag: String,
}

fn extract_if_match(req: &Request<'_>) -> Result<IfMatchHeader, Error> {
    use super::error::HttpError;
    let etag = req
        .headers()
        .get_one("If-Match")
        .ok_or(Error::HttpError(HttpError::NoIfMatchHeader))?;

    Ok(IfMatchHeader {
        etag: etag.to_string(),
    })
}

#[rocket::async_trait]
impl<'r> FromRequest<'r> for IfMatchHeader {
    type Error = super::error::Error;

    async fn from_request(req: &'r Request<'_>) -> Outcome<Self, Self::Error> {
        let result = extract_if_match(req);

        match result {
            Ok(e) => Outcome::Success(e),
            Err(e) => Outcome::Error((Status::Unauthorized, e)),
        }
    }
}

use hmac::{Hmac, Mac};
use jwt::{SignWithKey, VerifyWithKey};
use sha2::Sha256;
use std::collections::BTreeMap;
use std::env;

pub fn load_hmac_key() -> Result<Hmac<Sha256>, Error> {
    let secret = env::var("SECRET")?;
    let key: Hmac<Sha256> = Hmac::new_from_slice(secret.as_bytes())?;

    Ok(key)
}

pub fn _sign_token(sub: String) -> Result<String, Error> {
    let key = load_hmac_key()?;

    let mut claims = BTreeMap::new();
    claims.insert("sub", sub);
    let token_str = claims.sign_with_key(&key)?;

    Ok(token_str)
}

pub fn verify_token(token_str: &str) -> Result<BTreeMap<String, String>, Error> {
    let key = load_hmac_key()?;

    let claims: BTreeMap<String, String> = token_str.verify_with_key(&key)?;

    Ok(claims)
}

#[derive(Debug)]
pub struct JwtIdentifiedSubject {
    pub email: String,
}

fn extract_subject(req: &Request<'_>) -> Result<JwtIdentifiedSubject, Error> {
    use super::error::AuthError;
    let result = req
        .headers()
        .get_one("Authorization")
        .ok_or(Error::AuthError(AuthError::NoAuthorizationHeader))?;
    let result = if result.starts_with("Bearer ") {
        Ok(&result[7..])
    } else {
        Err(Error::AuthError(AuthError::NoBearerToken))
    }?;
    let result = verify_token(result)?;

    let result = result
        .get("sub")
        .ok_or(Error::AuthError(AuthError::NoSubjectClaim))?;
    let result = JwtIdentifiedSubject {
        email: result.to_string(),
    };

    Ok(result)
}

#[rocket::async_trait]
impl<'r> FromRequest<'r> for JwtIdentifiedSubject {
    type Error = super::error::Error;

    async fn from_request(req: &'r Request<'_>) -> Outcome<Self, Self::Error> {
        let result = extract_subject(req);

        match result {
            Ok(subject) => Outcome::Success(subject),
            Err(e) => Outcome::Error((Status::Unauthorized, e)),
        }
    }
}

pub struct ServerState {
    pub db_pool: DbPool,
    pub redis_pool: RedisPool,
    pub rmq_publisher: RmQPublisher,
}

pub async fn build_server() -> Result<Rocket<Build>, Error> {
    let db_pool = db::init_pool()?;
    let rmq_publisher = rmqpub::init_publisher().await?;
    let redis_pool = redis::init_pool()?;

    let server = rocket::build()
        .mount("/", routes![health])
        .mount("/api/", {
            routes![
                all_options,
                post::get_posts,
                post::get_post,
                post::post_post,
                post::delete_post,
                post::publish_post,
                post::patch_post,
                place::get_places,
                place::get_places_geojson,
            ]
        })
        .attach(Cors)
        .manage(ServerState {
            db_pool: db_pool,
            rmq_publisher: rmq_publisher,
            redis_pool: redis_pool,
        });

    Ok(server)
}

pub fn check_none_match(
    conn: &mut RedisConn,
    cache_key: &String,
    if_none_match_header: Option<IfNoneMatchHeader>,
) -> Result<(), Error> {
    let use_cache = if_none_match_header.map(|er| redis::match_etag(conn, cache_key, er.etag));
    match use_cache {
        Some(Ok(true)) => Err(HttpError::NotModified.into()),
        _ => Ok(()),
    }
    .map_err(|e: Error| e)?;

    Ok(())
}
pub fn check_match(
    conn: &mut RedisConn,
    cache_key: &String,
    if_match_header: Option<IfMatchHeader>,
) -> Result<(), Error> {
    let use_cache = if_match_header.map(|er| redis::match_etag(conn, cache_key, er.etag));
    match use_cache {
        None => Ok(()),
        Some(Ok(true)) => Ok(()),
        _ => Err(HttpError::PreconditionFailed.into()),
    }
    .map_err(|e: Error| e)?;

    Ok(())
}

pub fn resolve_agent(
    conn: &mut PgConnection,
    subject: Option<JwtIdentifiedSubject>,
) -> Result<Agent, Error> {
    match subject {
        None => Ok(Agent::Unknown),
        Some(s) => db::find_user(conn, s.email)?
            .ok_or(Error::from(HttpError::Unauthorized))
            .map(|u| Agent::User(u)),
    }
}

pub fn get_etag_safe(conn: &mut RedisConn, cache_key: &String) -> Option<String> {
    let etag = match redis::get_etag(conn, cache_key) {
        Ok(t) => Some(t),
        _ => None,
    };

    etag
}
