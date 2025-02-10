use application::models::{NewPost, Post};
use diesel::prelude::*;
use dotenvy::dotenv;
use std::env;

pub mod error;
mod models;
mod schema;

use self::error::Error;

pub fn establish_connection() -> Result<PgConnection, Error> {
    dotenv()?;
    let database_url = env::var("DATABASE_URL")?;
    let connection = PgConnection::establish(&database_url)?;

    Ok(connection)
}


pub fn get_published_posts() -> Result<Vec<application::models::Post>, Error> {
    use self::schema::posts::dsl::*;
    use self::models::Post;

    let connection = &mut establish_connection()?;
    let results: Vec<self::models::Post>= posts
        .filter(published.eq(true))
        .limit(5)
        .select(Post::as_select())
        .load(connection)?;
    let results: Vec<application::models::Post> = results.into_iter().map(|p| p.into()).collect();

    Ok(results)
}


pub fn get_post(post_id: i32) -> Result<Option<application::models::Post>, Error> {
    use self::schema::posts::dsl::*;
    use self::models::Post;

    let connection = &mut establish_connection()?;
    
    let result: Option<self::models::Post> = posts
        .find(post_id)
        .select(Post::as_select())
        .first(connection)
        .optional()?;
    let result = result.map(|p| p.into());

    Ok(result)
}

pub fn create_post(new_post: NewPost) -> Result<Post, Error> {
    use self::schema::posts;
    let connection = &mut establish_connection()?;

    let new_post: self::models::NewPost  = new_post.into();

    let result = diesel::insert_into(posts::table)
        .values(&new_post)
        .returning(self::models::Post::as_returning())
        .get_result(connection)?;

    Ok(result.into())

}

#[test]
fn test_connection() {
    let results = get_published_posts().expect("couldnt load posts");

    println!("Displaying {} posts", results.len());
    println!("-----------");
    for post in results {
        println!("{}", post.title);
        println!("> {}", post.body);
        println!("-----------");
    }
}