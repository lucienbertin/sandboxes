// use domain::models::NewPost;
// use domain::models::Post;
// use domain::models::PostEdition;


use crate::error::Error;

mod models;
mod schema;

use diesel::prelude::*;
use diesel::pg::PgConnection;
use diesel::r2d2::{self, ConnectionManager};
use ::r2d2::PooledConnection;
use rocket::http::Status;
use rocket::request::{self, FromRequest};
use rocket::{outcome::Outcome, Request, State};
use std::ops::Deref;
use dotenvy::dotenv;
use std::env;

// Alias to the type for a pool of Diesel PostgreSQL connections.
pub type Pool = r2d2::Pool<ConnectionManager<PgConnection>>;
pub type PooledConn = PooledConnection<ConnectionManager<PgConnection>>;
pub struct PoolState {
    pub pool: Pool
  }
// initializes a data pool
pub fn init_pool() -> Result<Pool, Error> {
    dotenv()?;
    let database_url = env::var("DATABASE_URL")?;
    let manager = ConnectionManager::<PgConnection>::new(database_url);
    let pool = diesel::r2d2::Pool::new(manager)?;
    
    Ok(pool)
}

// Connection request guard: a wrapper around an r2d2 pooled connection.
pub struct Conn(pub r2d2::PooledConnection<ConnectionManager<PgConnection>>);

/// Attempts to retrieve a single connection from the managed database pool. If
/// no pool is currently managed, fails with an `InternalServerError` status. If
/// no connections are available, fails with a `ServiceUnavailable` status.
// impl<'r> FromRequest<'r> for Conn {
//     type Error = super::error::Error;

//     fn from_request(request: &'r Request<'_>) -> request::Outcome<Conn, Self::Error> {
//         let pool = request.guard::<State<Pool>>();
//         match pool.get() {
//             Ok(conn) => Outcome::Success(Conn(conn)),
//             Err(e) => Outcome::Error((Status::ServiceUnavailable, e)),
//         }
//     }
// }

// For the convenience of using an &DbConn as an &PgConnection.
impl Deref for Conn {
    type Target = PgConnection;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub fn select_published_posts(connection: &mut PgConnection) -> Result<Vec<domain::models::Post>, Error> {
    use self::models::Post;
    use self::schema::posts::dsl::*;


    // let connection = &mut establish_connection()?;
    let results: Vec<self::models::Post> = posts
        .filter(published.eq(true))
        .limit(5)
        .select(Post::as_select())
        .load(connection)?;
    let results: Vec<domain::models::Post> = results.into_iter().map(|p| p.into()).collect();

    Ok(results)
}

// pub fn find_post(connection: &mut Connection<Db>, post_id: i32) -> Result<Option<domain::models::Post>, Error> {
//     use self::models::Post;
//     use self::schema::posts::dsl::*;

//     let result: Option<self::models::Post> = posts
//         .find(post_id)
//         .select(Post::as_select())
//         .first(connection)
//         .optional()?;
//     let result = result.map(|p| p.into());

//     Ok(result)
// }

// pub fn insert_new_post(connection: &mut PgConnection, new_post: NewPost) -> Result<Post, Error> {
//     use self::schema::posts;

//     let new_post: self::models::NewPost = new_post.into();

//     let result = diesel::insert_into(posts::table)
//         .values(&new_post)
//         .returning(self::models::Post::as_returning())
//         .get_result(connection)?;

//     Ok(result.into())
// }

// pub fn delete_post(connection: &mut PgConnection, post_id: i32) -> Result<(), Error> {
//     use self::schema::posts::dsl::*;

//     diesel::delete(posts.filter(id.eq(post_id))).execute(connection)?;

//     Ok(())
// }

// pub fn publish_post(connection: &mut PgConnection, post_id: i32) -> Result<(), Error> {
//     use self::schema::posts::dsl::*;

//     diesel::update(posts.filter(id.eq(post_id)))
//         .set(published.eq(true))
//         .execute(connection)?;

//     Ok(())
// }

// pub fn update_post(connection: &mut PgConnection, post_id: i32, edits: PostEdition) -> Result<(), Error> {
//     use self::schema::posts::dsl::*;

//     match edits {
//         PostEdition {
//             title: Some(t),
//             body: Some(b),
//         } => diesel::update(posts.filter(id.eq(post_id)))
//             .set((title.eq(t.as_str()), body.eq(b.as_str())))
//             .execute(connection),
//         PostEdition {
//             title: None,
//             body: Some(b),
//         } => diesel::update(posts.filter(id.eq(post_id)))
//             .set(body.eq(b.as_str()))
//             .execute(connection),
//         PostEdition {
//             title: Some(t),
//             body: None,
//         } => diesel::update(posts.filter(id.eq(post_id)))
//             .set(title.eq(t.as_str()))
//             .execute(connection),
//         PostEdition {
//             title: None,
//             body: None,
//         } => panic!("update_post called with nothing to update"),
//     }?;

//     Ok(())
// }
