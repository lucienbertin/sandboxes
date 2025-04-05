use crate::error::Error;
use dotenvy::dotenv;
use redis::Commands;
use std::env;

pub type RedisPool = r2d2::Pool<redis::Client>;
pub type RedisConn = r2d2::PooledConnection<redis::Client>;

pub fn init_pool()-> Result<RedisPool, Error>  {
    dotenv()?;
    let redis_url = env::var("REDIS_URL")?;
    let client = redis::Client::open(redis_url)?;
    let pool = r2d2::Pool::builder().build(client)?;

    Ok(pool)
}

pub fn get_conn(redis_pool: &RedisPool) -> Result<RedisConn, Error> {
    let conn: RedisConn = redis_pool.get()?;

    Ok(conn)
}

pub fn match_etag(conn: &mut RedisConn, key: String, etag: String) -> bool {
    let cached_etag: &Result<String, Error> = &conn.get(key).map_err(|e| e.into());

    match cached_etag {
        Ok(tag) if *tag == etag => true,
        Ok(_) => false,
        Err(_) => false, // dump error, we dont care
    }
}

use rocket::{
    http::Status,
    request::{FromRequest, Outcome},
    Request,
};
pub struct EtaggedRequest {
    pub etag: String,
}

fn extract_etag(req: &Request<'_>) -> Result<EtaggedRequest, Error> {
    use super::error::AuthError;
    let etag = req
        .headers()
        .get_one("If-None-Match")
        .ok_or(Error::AuthError(AuthError::NoAuthorizationHeader))?;

    Ok(EtaggedRequest { etag: etag.to_string() })
}

#[rocket::async_trait]
impl<'r> FromRequest<'r> for EtaggedRequest {
    type Error = super::error::Error;

    async fn from_request(req: &'r Request<'_>) -> Outcome<Self, Self::Error> {
        let result = extract_etag(req);

        match result {
            Ok(e) => Outcome::Success(e),
            Err(e) => Outcome::Error((Status::Unauthorized, e)),
        }
    }
}

mod test {
    use crate::error::Error;
    use dotenvy::dotenv;
    use redis::Commands;
    use std::env;
    fn do_something() -> Result<(), Error> {
        dotenv()?;
        let redis_url = env::var("REDIS_URL")?;

        let client = redis::Client::open(redis_url)?;
        let mut con = client.get_connection()?;

        /* do something here */
        con.set("posts", "qwe")?;
        let val: String = con.get("posts")?;
        println!("{:?}", val);

        Ok(())
    }

    #[test]
    fn test_redis() {
        do_something().expect("has done something");
    }
}
