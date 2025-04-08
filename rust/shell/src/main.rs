use api::Cors;
use db::DbPool;
use redis::RedisPool;
use rmq::RmqMessage;

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
    pub rmq_sender: std::sync::mpsc::Sender<RmqMessage>,
}
#[launch]
async fn rocket() -> _ {
    let rmq_sender = rmq::init()
        .await
        .expect("couldnt init rmq's consumer/publisher duo");
    let db_pool = db::init_pool().expect("couldnt init db pool");
    let redis_pool = redis::init_pool().expect("couldnt init redis pool");

    rocket::build()
        .mount("/api/", {
            use api::*;
            routes![
                all_options,
                get_posts,
                get_post,
                post_post,
                delete_post,
                publish_post,
                patch_post
            ]
        })
        .attach(Cors)
        .manage(ServerState {
            db_pool: db_pool,
            rmq_sender: rmq_sender,
            redis_pool: redis_pool,
        })
}
