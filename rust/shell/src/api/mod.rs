mod place;
mod post;

use crate::error::Error;
pub use place::*;
pub use post::*;

use rocket::fairing::{Fairing, Info, Kind};
use rocket::http::{ContentType, Header};
use rocket::request::Request;
use rocket::response::Responder;
use rocket::serde::json::Json;
use rocket::Response;
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
