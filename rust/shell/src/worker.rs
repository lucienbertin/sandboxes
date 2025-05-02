mod auth;
mod db;
mod error;
mod redis;
mod rmq;

#[tokio::main]
#[cfg(feature = "rmq-sub")]
async fn main() {
    use dotenvy::dotenv;
    match dotenv() {
        Ok(_) => println!("loaded local .env file"),
        Err(_) => println!("no local .env file to load"),
    };

    // let db_pool = db::init_pool().expect("couldnt init db pool");
    // let redis_pool = redis::init_pool().expect("couldnt init redis pool");
    rmq::start_consumer()
        .await
        .expect("couldnt start rmq's consumer");
}

#[cfg(not(feature = "rmq-sub"))]
fn main() {}