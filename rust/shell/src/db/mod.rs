mod post;
mod schema;
mod user;

pub use post::*;
pub use user::*;

use crate::error::Error;
use diesel::pg::PgConnection;
use diesel::r2d2::{self, ConnectionManager};
use dotenvy::dotenv;
use std::env;

// Alias to the type for a pool of Diesel PostgreSQL connections.
pub type DbPool = r2d2::Pool<ConnectionManager<PgConnection>>;

// initializes a data pool
pub fn init_pool() -> Result<DbPool, Error> {
    dotenv()?;
    let database_url = env::var("DATABASE_URL")?;
    let manager = ConnectionManager::<PgConnection>::new(database_url);
    let pool = diesel::r2d2::Pool::new(manager)?;

    Ok(pool)
}
