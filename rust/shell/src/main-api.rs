mod error;

#[macro_use]
#[cfg(feature = "api")]
extern crate rocket;

#[cfg(feature = "api")]
mod api;

#[cfg(feature = "db")]
mod db;

#[cfg(feature = "redis")]
mod redis;

#[cfg(feature = "rmqpub")]
mod rmqpub;

// REST API server using rocket
#[cfg(feature = "api")]
#[rocket::main]
async fn main() {
    use dotenvy::dotenv;
    match dotenv() {
        Ok(_) => println!("loaded local .env file"),
        Err(_) => println!("no local .env file to load"),
    };

    let server = api::build_server()
        .await
        .expect("couldn't build rocket server");

    let _ = server.launch().await;
}
