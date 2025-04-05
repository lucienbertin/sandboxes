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
    con.set("asd", "qwe")?;
    let val: String = con.get("asd")?;
    println!("{:?}", val);

    Ok(())
}

#[test]
fn test_redis() {
    do_something().expect("has done something");
}