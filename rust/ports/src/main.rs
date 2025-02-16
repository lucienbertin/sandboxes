#[macro_use]
extern crate rocket;

mod api;
mod auth;
mod error;
mod models;

#[launch]
fn rocket() -> _ {
    rocket::build().mount("/api/", {
        use api::*;
        routes![
            get_posts,
            get_post,
            post_post,
            delete_post,
            publish_post,
            patch_post
        ]
    })
}
