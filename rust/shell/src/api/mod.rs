use crate::auth::JwtIdentifiedSubject;
use crate::error::Error;

mod models;

use crate::db::{self, find_user, PoolState};

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
    _subject: Option<JwtIdentifiedSubject>, // is allowed with no auth
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
    _subject: Option<JwtIdentifiedSubject>, // is allowed with no auth
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
        let subject = find_user(conn, subject.email)?;
        let subject = subject.ok_or(Error::Unauthorized)?;
        let result = db::find_post(conn, id)?;
        let result = result.ok_or(Error::NotFound)?;

        use domain::usecases::{publish_post, PublishPostResult::*};
        let result = publish_post(&subject, &result);

        match result {
            CantPublishAnotherOnesPost => Err(Error::Forbidden),
            CantPublishAlreadyPublishedPost => Ok(()), // treat it as Ok for idempotency purpose
            DoPublish(post_id) => db::publish_post(conn, post_id),
            CantPublishAsReader => Err(Error::Forbidden),
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
        use domain::usecases::{create_post, CreatePostResult::*};

        let subject = find_user(conn, subject.email)?;
        let subject = subject.ok_or(Error::Unauthorized)?;
        
        let result = create_post(&subject, data.into_inner().into());
        match result {
            DoCreate(new_post) => db::insert_new_post(conn, new_post),
            CantCreateAsReader => Err(Error::Forbidden),
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
        let subject = find_user(conn, subject.email)?;
        let subject = subject.ok_or(Error::Unauthorized)?;

        let result = db::find_post(conn, id)?;
        let result = result.ok_or(Error::Gone)?;

        let result = domain::usecases::delete_post(&subject, &result);

        use domain::usecases::DeletePostResult::*;
        match result {
            DoDelete(post_id) => db::delete_post(conn, post_id),
            CantDeleteAnotherOnesPost => Err(Error::Forbidden),
            CantDeletePublishedPost => Err(Error::Conflict),
            CantDeleteAsReader => Err(Error::Forbidden),
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
        let subject = find_user(conn, subject.email)?;
        let subject = subject.ok_or(Error::Unauthorized)?;
        let result = db::find_post(conn, id)?;
        let result = result.ok_or(Error::NotFound)?;

        let result = domain::usecases::edit_post(&subject, &result, data.into_inner().into());

        use domain::usecases::EditPostResult::*;
        match result {
            DoUpdate(post_id, post_edition) => db::update_post(conn, post_id, post_edition),
            NothingToUpdate => Ok(()), // treat it as Ok for idempotency purpose
            CantEditAnotherOnesPost => Err(Error::Forbidden),
            CantEditPublishedPost => Err(Error::Conflict),
            CantEditAsReader => Err(Error::Forbidden),
        }
    })?;

    Ok(NoContent)
}
