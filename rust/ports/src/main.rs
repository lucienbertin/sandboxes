#[macro_use] extern crate rocket;
use models::NewPost;
use rocket::{form::Form, http::Status, response::status::Created, serde::json::Json};

pub mod models;

#[get("/posts", format = "json")]
pub fn get_posts() -> Result<Json<Vec<self::models::Post>>, rocket::response::status::Custom<&'static str>> {
    use adapters::get_published_posts;

    let results = get_published_posts().map_err(|_e| rocket::response::status::Custom(Status::InternalServerError, "error"))?;
    let results = results.into_iter().map(|x| x.into()).collect();

    Ok(Json(results))
}

#[get("/post/<id>", format = "json")]
pub fn get_post(id: i32) -> Result<Json<self::models::Post>, rocket::response::status::Custom<&'static str>> {
    use adapters::get_post;

    let result = get_post(id).map_err(|_e| rocket::response::status::Custom(Status::InternalServerError, "error"))?;

    match result {
        Some(p) => Ok(Json(p.into())),
        None => Err(rocket::response::status::Custom(Status::NotFound, "not found"))
    }
}

#[post("/posts", data = "<post>")]
pub fn post_post(post: Form<NewPost>) -> Result<Created<Json<self::models::Post>>, rocket::response::status::Custom<&'static str>> {
    use adapters::create_post;

    let result = create_post(post.into_inner().into()).map_err(|_e| rocket::response::status::Custom(Status::InternalServerError, "error"))?;
    let location = format!("/api/post/{}", result.id);

    Ok(Created::new(location))
}

#[launch]
fn rocket() -> _ {
    rocket::build()
        .mount("/api/", routes![get_posts, get_post, post_post])
}
