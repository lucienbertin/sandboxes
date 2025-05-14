mod error;

#[cfg(feature = "api")]
mod api;

#[cfg(feature = "db")]
mod db;

#[cfg(feature = "redis")]
mod redis;

#[cfg(feature = "rmqpub")]
mod rmqpub;

#[cfg(any(feature = "appssr", feature="appcsr"))]
mod app;

#[cfg(feature = "rmqsub")]
mod rmqsub;

// NOTHING
#[cfg(not(any(feature = "api", feature = "appssr", feature = "rmqsub")))]
fn main() {

}