use crate::auth::JwtIdentifiedSubject;
use crate::error::Error;

mod models;

use crate::db::{self, PoolState};

use self::models::{NewPost, PatchPost, Post};

use rocket::State;
use rocket::{
    form::Form,
    http::Status,
    response::status::{Created, NoContent},
    serde::json::Json,
};

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
    let results = conn
        .build_transaction()
        .read_only()
        .run(|conn| db::select_published_posts(conn))?;

    let results = results.into_iter().map(|x| x.into()).collect();

    Ok(Json(results))
}

#[get("/post/<id>", format = "json")]
pub fn get_post(
    state: &State<PoolState>,
    _subject: JwtIdentifiedSubject,
    id: i32,
) -> Result<Json<Post>, rocket::response::status::Custom<String>> {
    let mut conn = state.pool.get().map_err(|e| {
        rocket::response::status::Custom(
            Status::InternalServerError,
            format!("error: {:?}", e).to_string(),
        )
    })?;
    let result = conn
        .build_transaction()
        .read_only()
        .run(|conn| db::find_post(conn, id))?;

    match result {
        Some(p) => Ok(Json(p.into())),
        None => Err(rocket::response::status::Custom(
            Status::NotFound,
            "not found".to_string(),
        )),
    }
}

#[post("/post/<id>/publish")]
pub fn publish_post(
    state: &State<PoolState>,
    subject: JwtIdentifiedSubject,
    id: i32,
) -> Result<Status, rocket::response::status::Custom<String>> {
    let mut conn = state.pool.get().map_err(|e| {
        rocket::response::status::Custom(
            Status::InternalServerError,
            format!("error: {:?}", e).to_string(),
        )
    })?;

    conn.build_transaction().run(|conn| {
        let result = db::find_post(conn, id)?;

        let result = result.ok_or(Error::NotFound)?;

        use domain::usecases::{publish_post, PublishPostResult};
        let result = publish_post(&subject.email, &result);

        match result {
            PublishPostResult::CantPublishAnotherOnesPost => Err(Error::Forbidden),
            PublishPostResult::CantPublishAlreadyPublishedPost => Ok(()), // treat it as Ok for idempotency purpose
            PublishPostResult::DoPublish(post_id) => db::publish_post(conn, post_id),
        }
    })?;

    Ok(Status::NoContent)
}

#[post("/posts", data = "<data>")]
pub fn post_post(
    state: &State<PoolState>,
    subject: JwtIdentifiedSubject,
    data: Form<NewPost>,
) -> Result<Created<Json<Post>>, rocket::response::status::Custom<String>> {
    let mut conn = state.pool.get().map_err(|e| {
        rocket::response::status::Custom(
            Status::InternalServerError,
            format!("error: {:?}", e).to_string(),
        )
    })?;

    let result = conn.build_transaction().run(|conn| {
        use domain::usecases::{create_post, CreatePostResult};
        let result = create_post(&subject.email, data.into_inner().into());
        match result {
            CreatePostResult::DoCreate(new_post) => db::insert_new_post(conn, new_post),
        }
    })?;

    let result = format!("/api/post/{}", result.id);

    Ok(Created::new(result))
}

#[delete("/post/<id>")]
pub fn delete_post(
    state: &State<PoolState>,
    subject: JwtIdentifiedSubject,
    id: i32,
) -> Result<NoContent, rocket::response::status::Custom<String>> {
    let mut conn = state.pool.get().map_err(|e| {
        rocket::response::status::Custom(
            Status::InternalServerError,
            format!("error: {:?}", e).to_string(),
        )
    })?;

    conn.build_transaction().run(|conn| {
        let result = db::find_post(conn, id)?;
        let result = result.ok_or(Error::Gone)?;

        let result = domain::usecases::delete_post(&subject.email, &result);

        match result {
            domain::usecases::DeletePostResult::DoDelete(post_id) => db::delete_post(conn, post_id),
            domain::usecases::DeletePostResult::CantDeleteAnotherOnesPost => Err(Error::Forbidden),
            domain::usecases::DeletePostResult::CantDeletePublishedPost => Err(Error::Conflict),
        }
    })?;

    Ok(NoContent)
}

#[patch("/post/<id>", data = "<data>")]
pub fn patch_post(
    state: &State<PoolState>,
    subject: JwtIdentifiedSubject,
    id: i32,
    data: Form<PatchPost>,
) -> Result<NoContent, rocket::response::status::Custom<String>> {
    let mut conn = state.pool.get().map_err(|e| {
        rocket::response::status::Custom(
            Status::InternalServerError,
            format!("error: {:?}", e).to_string(),
        )
    })?;

    conn.build_transaction().run(|conn| {
        let result = db::find_post(conn, id)?;
        let result = result.ok_or(Error::NotFound)?;

        let result = domain::usecases::edit_post(&subject.email, &result, data.into_inner().into());

        match result {
            domain::usecases::EditPostResult::DoUpdate(post_id, post_edition) => {
                db::update_post(conn, post_id, post_edition)
            }
            domain::usecases::EditPostResult::NothingToUpdate => Ok(()), // treat it as Ok for idempotency purpose
            domain::usecases::EditPostResult::CantEditAnotherOnesPost => Err(Error::Forbidden),
            domain::usecases::EditPostResult::CantEditPublishedPost => Err(Error::Conflict),
        }
    })?;

    Ok(NoContent)
}
