use crate::error::Error;
use redis::Commands;
use std::env;

pub type RedisPool = r2d2::Pool<redis::Client>;
pub type RedisConn = r2d2::PooledConnection<redis::Client>;

const KEY_PREFIX: &str = "rust";
fn prefix_key(key: &String) -> String {
    format!("{}|{}", KEY_PREFIX, key)
}

pub fn init_pool() -> Result<RedisPool, Error> {
    let redis_url = env::var("REDIS_URL")?;
    let client = redis::Client::open(redis_url)?;
    let pool = r2d2::Pool::builder().build(client)?;

    Ok(pool)
}

pub fn get_conn(redis_pool: &RedisPool) -> Result<RedisConn, Error> {
    let conn: RedisConn = redis_pool.get()?;

    Ok(conn)
}

pub fn match_etag(conn: &mut RedisConn, key: &String, etag: String) -> Result<bool, Error> {
    let prefixed_key = prefix_key(key);

    let cached_etag = conn.get::<String, String>(prefixed_key)?;

    Ok(cached_etag == etag)
}

pub fn get_etag(conn: &mut RedisConn, key: &String) -> Result<String, Error> {
    let prefixed_key = prefix_key(key);
    let etag = conn.get::<String, Option<String>>(prefixed_key)?;
    let etag = match etag {
        Some(t) => t,
        None => refresh_etag(conn, &key)?,
    };

    Ok(etag)
}

pub fn refresh_etag(conn: &mut RedisConn, key: &String) -> Result<String, Error> {
    use rand::{distr::Alphanumeric, Rng}; // 0.8

    let prefixed_key = prefix_key(key);

    let etag: String = rand::rng()
        .sample_iter(&Alphanumeric)
        .take(12)
        .map(char::from)
        .collect();

    conn.set::<String, String, ()>(prefixed_key, etag.clone())?;

    Ok(etag)
}

use rocket::{
    http::Status,
    request::{FromRequest, Outcome},
    Request,
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

// mod test {
//     use crate::error::Error;
//     use dotenvy::dotenv;
//     use redis::Commands;
//     use std::env;
//     fn do_something() -> Result<(), Error> {
//         dotenv()?;
//         let redis_url = env::var("REDIS_URL")?;

//         let client = redis::Client::open(redis_url)?;
//         let mut con = client.get_connection()?;

//         /* do something here */
//         con.set("posts", "qwe")?;
//         let val: String = con.get("posts")?;
//         println!("{:?}", val);

//         Ok(())
//     }

//     #[test]
//     fn test_redis() {
//         do_something().expect("has done something");
//     }
// }
