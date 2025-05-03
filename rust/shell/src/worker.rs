mod error;
#[cfg(feature = "db")]
mod db;
#[cfg(feature = "redis")]
mod redis;
#[cfg(feature = "rmqsub")]
mod rmqsub;

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

#[cfg(not(feature = "rmqsub"))]
fn main() {}
