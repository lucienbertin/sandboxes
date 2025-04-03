use db::DbPool;
use lapin::Channel;

#[macro_use]
extern crate rocket;

mod api;
mod auth;
mod db;
mod error;
mod rmq;

pub struct ServerState {
    pub db_pool: DbPool,
    pub rmq_channel: Channel,
}
#[launch]
async fn rocket() -> _ {
    // init rmq
    // connect to rmq, create chan, queue and consumer
    // listen to queue `test`
    use futures_lite::stream::StreamExt;
    use lapin::{
        options::{BasicAckOptions, BasicConsumeOptions, QueueDeclareOptions},
        types::FieldTable,
    };

    let rmq_channel = rmq::init_channel().await.expect("couldnt connect to rmq");

    // all this should go somewhere else
    let queue = rmq_channel
        .queue_declare(
            "test",
            QueueDeclareOptions::default(),
            FieldTable::default(),
        )
        .await
        .expect("should create queue test");

    let mut consumer = rmq_channel
        .basic_consume(
            queue.name().as_str(),
            "test_consumer",
            BasicConsumeOptions::default(),
            FieldTable::default(),
        )
        .await
        .expect("should consume queue");

    async_global_executor::spawn(async move {
        while let Some(delivery) = consumer.next().await {
            let delivery = delivery.expect("error in consumer");
            println!(
                "delivery key:{:?} | msg: {:?}",
                delivery.routing_key.as_str(),
                String::from_utf8(delivery.data.clone())
            );
            delivery.ack(BasicAckOptions::default()).await.expect("ack");
        }
    })
    .detach();
    // untill here

    // init db
    let db_pool = db::init_pool().expect("couldnt init db pool");

    rocket::build()
        .mount("/api/", {
            use api::*;
            routes![
                get_posts,
                get_post,
                post_post,
                delete_post,
                publish_post,
                patch_post
            ]
        })
        .manage(ServerState {
            db_pool: db_pool,
            rmq_channel: rmq_channel,
        })
}
