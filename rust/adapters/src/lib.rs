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