#[macro_use] extern crate rocket;

mod models;
mod error;
mod auth;
mod api;

#[launch]
fn rocket() -> _ {
    rocket::build()
        .mount("/api/", {
            use api::*;
            routes![get_posts, get_post, post_post, delete_post, publish_post]
        })
}
