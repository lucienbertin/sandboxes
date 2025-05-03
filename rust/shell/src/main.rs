mod error;

#[macro_use]
extern crate rocket;

#[cfg(feature = "api")]
mod api;
#[cfg(feature = "api")]
use api::Cors;

#[cfg(feature = "api")]
mod auth;

#[cfg(feature = "db")]
mod db;
#[cfg(feature = "db")]
use db::DbPool;

#[cfg(feature = "redis")]
mod redis;
#[cfg(feature = "redis")]
use redis::RedisPool;

#[cfg(feature = "rmqpub")]
mod rmqpub;
#[cfg(feature = "rmqpub")]
use rmqpub::RmQPublisher;

pub struct ServerState {
    pub db_pool: DbPool,
    pub redis_pool: RedisPool,
    pub rmq_publisher: RmQPublisher,
}
#[rocket::main]
async fn main() {
    use dotenvy::dotenv;
    match dotenv() {
        Ok(_) => println!("loaded local .env file"),
        Err(_) => println!("no local .env file to load"),
    };

    let db_pool = db::init_pool().expect("couldnt init db pool");
    let rmq_publisher = rmqpub::init_publisher()
        .await
        .expect("couldnt init rmq's consumer/publisher duo");
    let redis_pool = redis::init_pool().expect("couldnt init redis pool");

    let _ = rocket::build()
        .mount("/", routes![api::health])
        .mount("/api/", {
            use api::*;
            routes![
                all_options,
                get_posts,
                get_post,
                post_post,
                delete_post,
                publish_post,
                patch_post,
                get_places,
                get_places_geojson,
            ]
        })
        .attach(Cors)
        .manage(ServerState {
            db_pool: db_pool,
            rmq_publisher: rmq_publisher,
            redis_pool: redis_pool,
        })
        .launch()
        .await;
}
