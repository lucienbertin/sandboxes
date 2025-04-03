use dotenvy::dotenv;
use lapin::{options::{BasicPublishOptions, ExchangeDeclareOptions}, types::FieldTable, BasicProperties, Channel, Connection, ConnectionProperties, ExchangeKind};
use std::env;
use crate::error::Error;

// initialize connection
pub async fn init_channel() -> Result<Channel, Error> {
    dotenv()?;
    let amqp_url = env::var("AMQP_URL")?;
    let exchange_name = env::var("RMQ_EXCHANGE")?;

    let conn = Connection::connect(
        &amqp_url,
        ConnectionProperties::default(),
    )
    .await?;

    let chan = conn.create_channel().await?;

    chan.exchange_declare(
        exchange_name.as_str(),
        ExchangeKind::Topic,
        ExchangeDeclareOptions::default(),
        FieldTable::default(),
    ).await?;

    Ok(chan)
}

pub async fn publish(chan: &Channel, routing_key: &str, msg: String) -> Result<(), Error> {
    dotenv()?;
    let exchange_name = env::var("RMQ_EXCHANGE")?;
    chan.basic_publish(
        exchange_name.as_str(),
        routing_key,
        BasicPublishOptions::default(),
        msg.as_bytes(),
        BasicProperties::default(),
    ).await?;

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