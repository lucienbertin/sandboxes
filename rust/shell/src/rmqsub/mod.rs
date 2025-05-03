mod place;

use crate::error::Error;

use crate::db::DbPool;
use crate::redis::RedisPool;
use futures_lite::StreamExt;
use lapin::{
    message::Delivery,
    options::{
        BasicAckOptions, BasicConsumeOptions, BasicRejectOptions, QueueBindOptions,
        QueueDeclareOptions,
    },
    types::FieldTable,
    Channel, Connection, ConnectionProperties,
};
use std::env;

// initialize connection
async fn init_channel(amqp_url: &String) -> Result<Channel, Error> {
    let conn = Connection::connect(&amqp_url, ConnectionProperties::default()).await?;

    let chan = conn.create_channel().await?;

    Ok(chan)
}

pub async fn start_consumer(db_pool: &DbPool, redis_pool: &RedisPool) -> Result<(), Error> {
    let amqp_url = env::var("AMQP_URL")?;
    let channel = init_channel(&amqp_url).await?;

    // consumer init
    // declare queue;
    let place_events_queue = channel
        .queue_declare(
            "rust-evt.place.#",
            QueueDeclareOptions {
                durable: true,
                ..Default::default()
            },
            FieldTable::default(),
        )
        .await?;
    //bind queue
    channel
        .queue_bind(
            place_events_queue.name().as_str(),
            "nextjs",
            "evt.place.#",
            QueueBindOptions::default(),
            FieldTable::default(),
        )
        .await?;

    let mut consumer = channel
        .basic_consume(
            place_events_queue.name().as_str(),
            "rust_consumer",
            BasicConsumeOptions::default(),
            FieldTable::default(),
        )
        .await?;

    println!("rmq consumer started and awaiting messages");
    while let Some(rd) = consumer.next().await {
        let _ = handle_delivery(db_pool, redis_pool, rd).await; // fire n forget result
    }

    Ok(())
}

async fn handle_delivery(
    db_pool: &DbPool,
    redis_pool: &RedisPool,
    r: Result<Delivery, lapin::Error>,
) -> Result<(), Error> {
    use place::handle_create_place;
    let delivery = r.map_err(|e| Error::from(e))?;

    let result = match delivery.routing_key.as_str() {
        "evt.place.created" => handle_create_place(db_pool, redis_pool, &delivery),
        _ => log_delivery(&delivery), // just log and go to ack
    };

    match result {
        Ok(()) => delivery.ack(BasicAckOptions::default()).await,
        Err(_) => delivery.reject(BasicRejectOptions { requeue: false }).await,
    }
    .map_err(|e| Error::from(e))
}
fn log_delivery(delivery: &Delivery) -> Result<(), Error> {
    println!(
        "Delivery with key: {:?} recieved, but there are no handlers for it so it'll just be acked",
        delivery.routing_key.as_str()
    );
    println!("msg body: {:?}", String::from_utf8(delivery.data.clone()));

    Ok(())
}

#[cfg(test)]
mod tests {
    // use futures_lite::stream::StreamExt;
    // use super::init_channel;
    // use lapin::{options::{BasicAckOptions, BasicConsumeOptions, BasicPublishOptions, QueueDeclareOptions}, types::FieldTable, BasicProperties, Connection, ConnectionProperties};

    //     #[tokio::test]
    //     async fn test_rmq_conn() {
    //         let _conn = init_connection().await.expect("should connect to rmq");
    //     }

    //     #[tokio::test]
    //     async fn test_rmq_queue() {
    //         let conn = init_connection().await.expect("should connect to rmq");

    //         let chan = conn.create_channel().await.expect("should create channel");

    //         let _queue = chan.queue_declare(
    //             "test",
    //             QueueDeclareOptions::default(),
    //             FieldTable::default(),
    //         ).await.expect("should create queue test");
    //     }

    // #[tokio::test]
    // async fn test_rmq_publish() {
    //     let conn = init_channel().await.expect("should connect to rmq");

    //     let chan = conn.create_channel().await.expect("should create channel");

    //     let queue = chan.queue_declare(
    //         "test",
    //         QueueDeclareOptions::default(),
    //         FieldTable::default(),
    //     ).await.expect("should create queue test");

    //     let _publish = chan.basic_publish(
    //         "",
    //         queue.name().as_str(),
    //         BasicPublishOptions::default(),
    //         b"Hello world!",
    //         BasicProperties::default(),
    //     ).await.expect("should publish in channel");
    // }

    //     #[tokio::test]
    //     async fn test_rmq_consume() {
    //         let conn = init_connection().await.expect("should connect to rmq");

    //         let chan = conn.create_channel().await.expect("should create channel");

    //         let queue = chan.queue_declare(
    //             "test",
    //             QueueDeclareOptions::default(),
    //             FieldTable::default(),
    //         ).await.expect("should create queue test");

    //         let mut consumer = chan.basic_consume(
    //             queue.name().as_str(),
    //             "test_consumer",
    //             BasicConsumeOptions::default(),
    //             FieldTable::default(),
    //         ).await.expect("should consume queue");

    //         if let Some(delivery) = consumer.next().await {
    //             let delivery = delivery.expect("error in consumer");
    //             println!("delivery {:?}",delivery);
    //             delivery
    //                 .ack(BasicAckOptions::default())
    //                 .await
    //                 .expect("ack");
    //         }
    //     }
    //     #[tokio::test]
    //     async fn test_rmq_publish_n_consume() {
    //         let conn = init_connection().await.expect("should connect to rmq");

    //         let chan = conn.create_channel().await.expect("should create channel");

    //         let queue = chan.queue_declare(
    //             "test",
    //             QueueDeclareOptions::default(),
    //             FieldTable::default(),
    //         ).await.expect("should create queue test");

    //         let mut consumer = chan.basic_consume(
    //             queue.name().as_str(),
    //             "test_consumer",
    //             BasicConsumeOptions::default(),
    //             FieldTable::default(),
    //         ).await.expect("should consume queue");

    //         async_global_executor::spawn(async move {
    //             while let Some(delivery) = consumer.next().await {
    //                 let delivery = delivery.expect("error in consumer");
    //                 println!("delivery {:?}",delivery);
    //                 delivery
    //                     .ack(BasicAckOptions::default())
    //                     .await
    //                     .expect("ack");
    //             }
    //         }).detach();

    //         let _publish = chan.basic_publish(
    //             "",
    //             queue.name().as_str(),
    //             BasicPublishOptions::default(),
    //             b"Hello world!",
    //             BasicProperties::default(),
    //         ).await.expect("should publish in channel");

    //     }
}
