#[macro_use] extern crate rocket;
use rocket::{http::Status, serde::json::Json};

pub mod models;

#[get("/")]
fn index() -> &'static str {
    "Hello, world!"
}

#[get("/posts", format = "json")]
pub fn get_posts() -> Result<Json<Vec<self::models::Post>>, rocket::response::status::Custom<&'static str>> {
    use adapters::get_published_posts;

    let results = get_published_posts().map_err(|_e| rocket::response::status::Custom(Status::BadRequest, "error"))?;
    let results = results.into_iter().map(|x| x.into()).collect();

    Ok(Json(results))
}

#[launch]
fn rocket() -> _ {
    rocket::build()
        .mount("/", routes![index, get_posts])
        // .mount("/api/", routes![get_posts])
}
