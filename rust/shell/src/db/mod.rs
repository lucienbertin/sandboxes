mod post;
mod schema;
mod user;

pub use post::*;
pub use user::*;

use crate::error::Error;
use diesel::pg::PgConnection;
use diesel::r2d2::{self, ConnectionManager};
use std::env;

// Alias to the type for a pool of Diesel PostgreSQL connections.
pub type DbPool = r2d2::Pool<ConnectionManager<PgConnection>>;
pub type DbConn = r2d2::PooledConnection<ConnectionManager<PgConnection>>;

// initializes a data pool
pub fn init_pool() -> Result<DbPool, Error> {
    let database_url = env::var("DATABASE_URL")?;
    let manager = ConnectionManager::<PgConnection>::new(database_url);
    let pool = diesel::r2d2::Pool::new(manager)?;

    Ok(pool)
}

pub fn get_conn(db_pool: &DbPool) -> Result<DbConn, Error> {
    let conn: DbConn = db_pool.get()?;

    Ok(conn)
}
