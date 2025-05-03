mod job;
mod post;

pub use job::*;
pub use post::*;

use crate::error::Error;
use lapin::{
    options::{BasicPublishOptions, ExchangeDeclareOptions},
    types::FieldTable,
    BasicProperties, Channel, Connection, ConnectionProperties, ExchangeKind,
};

use std::{env, sync::mpsc};

async fn init_channel() -> Result<Channel, Error> {
    let amqp_url = env::var("AMQP_URL")?;
    let conn = Connection::connect(&amqp_url, ConnectionProperties::default()).await?;

    let chan = conn.create_channel().await?;

    Ok(chan)
}
async fn init_exchange(chan: &Channel) -> Result<String, Error> {
    let exchange_name = env::var("RMQ_EXCHANGE")?;

    chan.exchange_declare(
        exchange_name.as_str(),
        ExchangeKind::Topic,
        ExchangeDeclareOptions::default(),
        FieldTable::default(),
    )
    .await?;

    Ok(exchange_name)
}

async fn publish(
    chan: &Channel,
    exchange_name: &String,
    routing_key: String,
    message: String,
) -> Result<(), Error> {
    chan.basic_publish(
        exchange_name.as_str(),
        routing_key.as_str(),
        BasicPublishOptions::default(),
        message.as_bytes(),
        BasicProperties::default(),
    )
    .await?;

    Ok(())
}

pub struct RmQPublisher {
    sender: std::sync::mpsc::Sender<RmqMessage>,
}
impl RmQPublisher {
    fn new(sender: std::sync::mpsc::Sender<RmqMessage>) -> Self {
        Self { sender: sender }
    }

    pub fn publish(&self, routing_key: String, message: String) -> Result<(), Error> {
        self.sender.send((routing_key, message))?;

        Ok(())
    }
}

pub type RmqMessage = (String, String);
pub async fn init_publisher() -> Result<RmQPublisher, Error> {

    let channel = init_channel().await?;
    let exchange_name = init_exchange(&channel).await?;

    let (tx, rx) = mpsc::channel::<RmqMessage>();
    
    // publisher thread
    let channel_clone = channel.clone();
    async_global_executor::spawn(async move {
        for (routing_key, message) in rx {
            let _ = publish(&channel_clone, &exchange_name, routing_key, message).await;
        }
    })
    .detach();

    Ok(RmQPublisher::new(tx))
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
