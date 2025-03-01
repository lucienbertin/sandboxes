use crate::{auth::JwtIdentifiedSubject};
use crate::error::Error;

mod models;

use crate::db::{self, Pool, PoolState};

use self::models::{NewPost, PatchPost, Post};

use diesel::Connection;
use rocket::State;
use rocket::{
    form::Form,
    http::Status,
    response::status::{Created, NoContent},
    serde::json::Json,
};


#[get("/")]
pub fn index() -> &'static str {
    "Hello, world!"
}


#[get("/posts", format = "json")]
pub async fn get_posts(
    state: &State<PoolState>,
    _subject: JwtIdentifiedSubject,
) -> Result<Json<Vec<Post>>, rocket::response::status::Custom<String>> {
    let mut conn = state.pool.get().map_err(|e| {
        rocket::response::status::Custom(
            Status::InternalServerError,
            format!("error: {:?}", e).to_string(),
        )
    })?;
    let results = conn.build_transaction()
        .read_only()
        .run(|conn| {
            db::select_published_posts(conn)
        })?;

    let results = results.into_iter().map(|x| x.into()).collect();

    Ok(Json(results))
}

// #[get("/post/<id>", format = "json")]
// pub fn get_post(id: i32) -> Result<Json<Post>, rocket::response::status::Custom<String>> {
//     use db::find_post;

//     let result = find_post(id).map_err(|e| {
//         rocket::response::status::Custom(
//             Status::InternalServerError,
//             format!("error: {:?}", e).to_string(),
//         )
//     })?;

//     match result {
//         Some(p) => Ok(Json(p.into())),
//         None => Err(rocket::response::status::Custom(
//             Status::NotFound,
//             "not found".to_string(),
//         )),
//     }
// }

// #[post("/post/<id>/publish")]
// pub fn publish_post(
//     subject: JwtIdentifiedSubject,
//     id: i32,
// ) -> Result<Status, rocket::response::status::Custom<String>> {
//     use db::find_post;
//     use core::usecases::{publish_post, PublishPostResult};

//     let result = find_post(id).map_err(|e| {
//         rocket::response::status::Custom(
//             Status::InternalServerError,
//             format!("error: {:?}", e).to_string(),
//         )
//     })?;
//     let result = result.ok_or(rocket::response::status::Custom(
//         Status::NotFound,
//         "not found".to_string(),
//     ))?;

//     let result = publish_post(&subject.email, &result);

//     match result {
//         PublishPostResult::CantPublishAnotherOnesPost => Err(rocket::response::status::Custom(
//             Status::Forbidden,
//             "you are not allowed to publish this post".to_string(),
//         )),
//         PublishPostResult::CantPublishAlreadyPublishedPost => Ok(()), // treat it as Ok for idempotency purpose
//         PublishPostResult::DoPublish(post_id) => {
//             db::publish_post(post_id).map_err(|e| Error::from(e).into())
//         }
//     }?;

//     Ok(Status::NoContent)
// }

// #[post("/posts", data = "<data>")]
// pub fn post_post(
//     subject: JwtIdentifiedSubject,
//     data: Form<NewPost>,
// ) -> Result<Created<Json<Post>>, rocket::response::status::Custom<String>> {
//     use db::insert_new_post;
//     use core::usecases::{create_post, CreatePostResult};

//     let result = create_post(&subject.email, data.into_inner().into());

//     let result = match result {
//         CreatePostResult::DoCreate(new_post) => insert_new_post(new_post).map_err(|e| {
//             rocket::response::status::Custom(
//                 Status::InternalServerError,
//                 format!("error: {:?}", e).to_string(),
//             )
//         }),
//     }?;

//     let result = format!("/api/post/{}", result.id);

//     Ok(Created::new(result))
// }

// #[delete("/post/<id>")]
// pub fn delete_post(
//     subject: JwtIdentifiedSubject,
//     id: i32,
// ) -> Result<NoContent, rocket::response::status::Custom<String>> {
//     use db::{delete_post, find_post};
//     use core::usecases::DeletePostResult;
//     // imperative shell
//     let post = find_post(id).map_err(|e| {
//         rocket::response::status::Custom(
//             Status::InternalServerError,
//             format!("error: {:?}", e).to_string(),
//         )
//     })?;

//     //functional core
//     let result = post.map(|p| core::usecases::delete_post(&subject.email, &p));

//     // imperative shell
//     match result {
//         Some(DeletePostResult::DoDelete(post_id)) => delete_post(post_id).map_err(|e| {
//             rocket::response::status::Custom(
//                 Status::InternalServerError,
//                 format!("error: {:?}", e).to_string(),
//             )
//         }),
//         Some(DeletePostResult::CantDeleteAnotherOnesPost) => Err(rocket::response::status::Custom(
//             Status::Forbidden,
//             "cant delete another one's post".to_string(),
//         )),
//         Some(DeletePostResult::CantDeletePublishedPost) => Err(rocket::response::status::Custom(
//             Status::Conflict,
//             "cant delete published post".to_string(),
//         )),
//         None => Err(rocket::response::status::Custom(
//             Status::Gone,
//             "gone".to_string(),
//         )),
//     }?;

//     Ok(NoContent)
// }

// #[patch("/post/<id>", data = "<data>")]
// pub fn patch_post(
//     subject: JwtIdentifiedSubject,
//     id: i32,
//     data: Form<PatchPost>,
// ) -> Result<NoContent, rocket::response::status::Custom<String>> {
//     use db::{find_post, update_post};
//     use core::usecases::EditPostResult;

//     let post = find_post(id).map_err(|e| {
//         rocket::response::status::Custom(
//             Status::InternalServerError,
//             format!("error: {:?}", e).to_string(),
//         )
//     })?;

//     let result = post
//         .map(|p| core::usecases::edit_post(&subject.email, &p, data.into_inner().into()));

//     match result {
//         Some(EditPostResult::DoUpdate(post_id, post_edition)) => update_post(post_id, post_edition)
//             .map_err(|e| {
//                 rocket::response::status::Custom(
//                     Status::InternalServerError,
//                     format!("error: {:?}", e).to_string(),
//                 )
//             }),
//         Some(EditPostResult::NothingToUpdate) => Ok(()), // treat it as Ok for idempotency purpose
//         Some(EditPostResult::CantEditAnotherOnesPost) => Err(rocket::response::status::Custom(
//             Status::Forbidden,
//             "cant edit another one's post".to_string(),
//         )),
//         Some(EditPostResult::CantEditPublishedPost) => Err(rocket::response::status::Custom(
//             Status::Conflict,
//             "cant edit published post".to_string(),
//         )),
//         None => Err(rocket::response::status::Custom(
//             Status::NotFound,
//             "not found".to_string(),
//         )),
//     }?;

//     Ok(NoContent)
// }
