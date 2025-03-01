use db::PoolState;


#[macro_use]
extern crate rocket;

mod db;
mod api;
mod auth;
mod error;

#[launch]
fn rocket() -> _ {
    let pool = db::init_pool().expect("couldnt init db pool");

    rocket::build()
        .mount("/api/", {
            use api::*;
            routes![
                index,
                get_posts,
                // get_post,
                // post_post,
                // delete_post,
                // publish_post,
                // patch_post
            ]
        })
        .manage(PoolState{ pool })

}
