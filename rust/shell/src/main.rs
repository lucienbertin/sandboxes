use api::Cors;
use db::DbPool;
use redis::RedisPool;
use rmq::RmQPublisher;

#[macro_use]
extern crate rocket;

mod api;
mod auth;
mod db;
mod error;
mod redis;
mod rmq;

pub struct ServerState {
    pub db_pool: DbPool,
    pub redis_pool: RedisPool,
    pub rmq_publisher: RmQPublisher,
}
#[launch]
async fn rocket() -> _ {
    use dotenvy::dotenv;
    match dotenv() {
        Ok(_) => println!("loaded local .env file"),
        Err(_) => println!("no local .env file to load"),
    };

    let db_pool = db::init_pool().expect("couldnt init db pool");
    let rmq_publisher = rmq::init()
        .await
        .expect("couldnt init rmq's consumer/publisher duo");
    let redis_pool = redis::init_pool().expect("couldnt init redis pool");

    rocket::build()
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
}
