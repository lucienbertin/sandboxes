mod error;

#[macro_use]
extern crate rocket;

#[cfg(feature = "db")]
mod db;

#[cfg(feature = "redis")]
mod redis;

#[cfg(feature = "rmqsub")]
mod rmqsub;


// RMQ subscriber worker using lapin
#[tokio::main]
#[cfg(feature = "rmqsub")]
async fn main() {
    use dotenvy::dotenv;
    match dotenv() {
        Ok(_) => println!("loaded local .env file"),
        Err(_) => println!("no local .env file to load"),
    };

    let db_pool = db::init_pool().expect("couldnt init db pool");
    let redis_pool = redis::init_pool().expect("couldnt init redis pool");
    rmqsub::start_consumer(&db_pool, &redis_pool)
        .await
        .expect("couldnt start rmq's consumer");
}

