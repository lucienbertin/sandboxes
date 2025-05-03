use super::ServerState;
use super::error::ResponseError;
use crate::db::{self, find_user};
use crate::error::{Error, HttpError};
use crate::redis::{self, match_etag};
use crate::rmqpub::{self};
use domain::models::Agent;
use rocket::serde::{Deserialize, Serialize};
use rocket::State;
use rocket::{
    form::Form,
    http::Status,
    response::status::{Created, NoContent},
    serde::json::Json,
};

use super::{EtagJson, IfMatchHeader, IfNoneMatchHeader, JwtIdentifiedSubject};

#[derive(Serialize, Deserialize)]
pub struct User {
    // pub id: i32,
    pub first_name: String,
    pub last_name: String,
    pub email: String,
}
#[derive(Serialize, Deserialize)]
pub struct Post {
    pub id: i32,
    pub title: String,
    pub body: String,
    pub published: bool,
    pub author: User,
}

impl From<domain::models::User> for User {
    fn from(value: domain::models::User) -> Self {
        Self {
            email: value.email,
            first_name: value.first_name,
            last_name: value.last_name,
        }
    }
}

impl From<domain::models::Post> for Post {
    fn from(value: domain::models::Post) -> Self {
        Self {
            id: value.id,
            title: value.title,
            body: value.body,
            published: value.published,
            author: value.author.into(),
        }
    }
}

#[derive(FromForm)]
pub struct NewPost {
    title: String,
    body: String,
}
impl From<NewPost> for domain::models::NewPostRequest {
    fn from(value: NewPost) -> Self {
        Self {
            title: value.title,
            body: value.body,
        }
    }
}

#[derive(FromForm)]
pub struct PatchPost {
    title: Option<String>,
    body: Option<String>,
}
impl From<PatchPost> for domain::models::PostEdition {
    fn from(value: PatchPost) -> Self {
        Self {
            title: value.title,
            body: value.body,
        }
    }
}

#[get("/posts", format = "json")]
pub async fn get_posts(
    server_state: &State<ServerState>,
    subject: JwtIdentifiedSubject, // is allowed with no auth
    if_none_match: Option<IfNoneMatchHeader>,
) -> Result<EtagJson<Vec<Post>>, ResponseError> {
    let cache_key = "posts".to_string();
    let mut redis_conn = redis::get_conn(&server_state.redis_pool)?;
    let use_cache = if_none_match.map(|er| match_etag(&mut redis_conn, &cache_key, er.etag));
    match use_cache {
        Some(Ok(true)) => Err(HttpError::NotModified.into()),
        _ => Ok(()),
    }.map_err(|e: Error| e)?;

    let mut db_conn = db::get_conn(&server_state.db_pool)?;

    let results = db_conn.build_transaction().read_only().run(|conn| -> Result<Vec<domain::models::Post>, Error> {
        let agent = find_user(conn, subject.email)?;
        let agent = agent.ok_or(HttpError::Unauthorized)?;
        let agent = Agent::User(agent);

        let result = domain::usecases::consult_posts(&agent);

        use domain::usecases::ConsultPostsResult::*;
        match result {
            ConsultPublishedPosts => db::select_published_posts(conn),
            ConsultPublishedPostsAndAuthoredBy(author) => {
                db::select_published_posts_or_authored_by(conn, author)
            }
            ConsultAllPosts => db::select_posts(conn),
        }
    })?;

    let results = results.into_iter().map(|x| x.into()).collect();
    let etag = match redis::get_etag(&mut redis_conn, &cache_key) {
        Ok(t) => Some(t),
        _ => None, // i dont want to 500 on a redis error when i have the results available
    };

    let result = EtagJson {
        body: Json(results),
        etag: etag,
    };

    Ok(result)
}

#[get("/post/<id>", format = "json")]
pub fn get_post(
    server_state: &State<ServerState>,
    subject: JwtIdentifiedSubject,
    id: i32,
    if_none_match: Option<IfNoneMatchHeader>,
) -> Result<EtagJson<Post>, ResponseError> {
    let cache_key = format!("posts.{}", id).to_string();
    let mut redis_conn = redis::get_conn(&server_state.redis_pool)?;
    let use_cache = if_none_match.map(|er| match_etag(&mut redis_conn, &cache_key, er.etag));
    match use_cache {
        Some(Ok(true)) => Err(HttpError::NotModified.into()),
        _ => Ok(()),
    }.map_err(|e: Error| e)?;

    let mut conn = db::get_conn(&server_state.db_pool)?;

    let result = conn.build_transaction().read_only().run(|conn| -> Result<domain::models::Post, Error> {
        let agent = find_user(conn, subject.email)?;
        let agent = agent.ok_or(HttpError::Unauthorized)?;
        let agent = Agent::User(agent);
        let post = db::find_post(conn, id)?;
        let post = post.ok_or(HttpError::NotFound)?;

        let result = domain::usecases::consult_post(&agent, &post);

        use domain::usecases::ConsultPostResult::*;
        match result {
            DoConsultPost(p) => Ok(p),
            CantConsultUnpublishedPostFromSomeoneElse => Err(HttpError::Forbidden.into()),
            CantConsultUnpublishedPostAsReader => Err(HttpError::Forbidden.into()),
        }
    })?;

    let etag = match redis::get_etag(&mut redis_conn, &cache_key) {
        Ok(t) => Some(t),
        _ => None, // i dont want to 500 on a redis error when i have the results available
    };
    let result = EtagJson {
        body: Json(result.into()),
        etag: etag,
    };

    Ok(result)
}

#[post("/post/<id>/publish")]
pub async fn publish_post(
    server_state: &State<ServerState>,
    subject: JwtIdentifiedSubject,
    id: i32,
    if_match: Option<IfMatchHeader>,
) -> Result<Status, ResponseError> {
    let cache_key = format!("posts.{}", id).to_string();
    let mut redis_conn = redis::get_conn(&server_state.redis_pool)?;
    let use_cache = if_match.map(|er| match_etag(&mut redis_conn, &cache_key, er.etag));
    match use_cache {
        None => Ok(()),
        Some(Ok(true)) => Ok(()),
        _ => Err(HttpError::PreconditionFailed.into()),
    }.map_err(|e: Error| e)?;

    let mut conn = db::get_conn(&server_state.db_pool)?;
    let rmq_publisher = &server_state.rmq_publisher;

    conn.build_transaction().run(move |conn| -> Result<(), Error> {
        let agent = find_user(conn, subject.email)?;
        let agent = agent.ok_or(HttpError::Unauthorized)?;
        let agent = Agent::User(agent);
        let post = db::find_post(conn, id)?;
        let post = post.ok_or(HttpError::NotFound)?;

        use domain::usecases::{publish_post, PublishPostResult::*};
        let result = publish_post(&agent, &post);

        match result {
            CantPublishAnotherOnesPost => Err(HttpError::Forbidden.into()),
            CantPublishAsReader => Err(HttpError::Forbidden.into()),
            CantPublishAsWorker => Err(HttpError::Forbidden.into()), // should never happen
            CantPublishAlreadyPublishedPost => Ok(()),    // treat it as Ok for idempotency purpose
            DoPublishAndNotify(post_id, post) => {
                db::publish_post(conn, post_id)?;

                let _ = rmqpub::notify_post_published(&rmq_publisher, &post);

                let cache_keys = ["posts".to_string(), format!("posts.{}", id).to_string()];
                let mut redis_conn = redis::get_conn(&server_state.redis_pool)?;
                for cache_key in cache_keys {
                    redis::refresh_etag(&mut redis_conn, &cache_key)?;
                }

                Ok(())
            }
            DoPublishNotifyAndSendMailToAuthor(post_id, post, author) => {
                db::publish_post(conn, post_id)?;
                let _ = rmqpub::notify_post_published(&rmq_publisher, &post);

                let cache_keys = ["posts".to_string(), format!("posts.{}", id).to_string()];
                let mut redis_conn = redis::get_conn(&server_state.redis_pool)?;
                for cache_key in cache_keys {
                    redis::refresh_etag(&mut redis_conn, &cache_key)?;
                }

                let _ = rmqpub::trigger_mail_post_published(&rmq_publisher, &post, &author);

                Ok(())
            }
        }
    })?;

    Ok(Status::NoContent)
}

#[post("/posts", data = "<data>")]
pub fn post_post(
    server_state: &State<ServerState>,
    subject: JwtIdentifiedSubject,
    data: Form<NewPost>,
    if_match: Option<IfMatchHeader>,
) -> Result<Created<Json<Post>>, ResponseError> {
    let cache_key = "posts".to_string();
    let mut redis_conn = redis::get_conn(&server_state.redis_pool)?;
    let use_cache = if_match.map(|er| match_etag(&mut redis_conn, &cache_key, er.etag));
    match use_cache {
        None => Ok(()),
        Some(Ok(true)) => Ok(()),
        _ => Err(HttpError::PreconditionFailed.into()),
    }.map_err(|e: Error| e)?;

    let mut conn = db::get_conn(&server_state.db_pool)?;

    let result = conn.build_transaction().run(|conn| -> Result<i32, Error> {
        use domain::usecases::{create_post, CreatePostResult::*};

        let agent = find_user(conn, subject.email)?;
        let agent = agent.ok_or(HttpError::Unauthorized)?;
        let agent = Agent::User(agent);

        let result = create_post(&agent, data.into_inner().into());
        match result {
            DoCreate(new_post) => {
                let post_id = db::insert_new_post(conn, new_post)?;

                let cache_key = "posts".to_string();
                let mut redis_conn = redis::get_conn(&server_state.redis_pool)?;
                redis::refresh_etag(&mut redis_conn, &cache_key)?;

                Ok(post_id)
            }
            CantCreateAsReader | CantCreateAsWorker => Err(HttpError::Forbidden.into()),
        }
    })?;

    let result = format!("/api/post/{}", result);

    Ok(Created::new(result))
}

#[delete("/post/<id>")]
pub fn delete_post(
    server_state: &State<ServerState>,
    subject: JwtIdentifiedSubject,
    if_match: Option<IfMatchHeader>,
    id: i32,
) -> Result<NoContent, ResponseError> {
    let cache_key = format!("posts.{}", id).to_string();
    let mut redis_conn = redis::get_conn(&server_state.redis_pool)?;
    let use_cache = if_match.map(|er| match_etag(&mut redis_conn, &cache_key, er.etag));
    match use_cache {
        None => Ok(()),
        Some(Ok(true)) => Ok(()),
        _ => Err(HttpError::PreconditionFailed.into()),
    }.map_err(|e: Error| e)?;

    let mut conn = db::get_conn(&server_state.db_pool)?;
    let rmq_publisher = &server_state.rmq_publisher;

    conn.build_transaction().run(|conn| -> Result<(), Error> {
        let agent = find_user(conn, subject.email)?;
        let agent = agent.ok_or(HttpError::Unauthorized)?;
        let agent = Agent::User(agent);

        let result = db::find_post(conn, id)?;
        let result = result.ok_or(HttpError::Gone)?;

        let result = domain::usecases::delete_post(&agent, &result);

        use domain::usecases::DeletePostResult::*;
        match result {
            CantDeleteAnotherOnesPost => Err(HttpError::Forbidden.into()),
            CantDeletePublishedPost => Err(HttpError::Conflict.into()),
            CantDeleteAsReader => Err(HttpError::Forbidden.into()),
            CantDeleteAsWorker => Err(HttpError::Forbidden.into()),
            DoDelete(post_id) => {
                db::delete_post(conn, post_id)?;

                let cache_keys = ["posts".to_string(), format!("posts.{}", id).to_string()];
                for cache_key in cache_keys {
                    redis::refresh_etag(&mut redis_conn, &cache_key)?;
                }

                Ok(())
            }
            DoDeleteAndNotify(post_id, post) => {
                db::delete_post(conn, post_id)?;

                let _ = rmqpub::notify_post_deleted(&rmq_publisher, &post);

                let cache_keys = ["posts".to_string(), format!("posts.{}", id).to_string()];
                for cache_key in cache_keys {
                    redis::refresh_etag(&mut redis_conn, &cache_key)?;
                }

                Ok(())
            }
        }
    })?;

    Ok(NoContent)
}

#[patch("/post/<id>", data = "<data>")]
pub fn patch_post(
    server_state: &State<ServerState>,
    subject: JwtIdentifiedSubject,
    if_match: Option<IfMatchHeader>,
    id: i32,
    data: Form<PatchPost>,
) -> Result<NoContent, ResponseError> {
    let cache_key = format!("posts.{}", id).to_string();
    let mut redis_conn = redis::get_conn(&server_state.redis_pool)?;
    let use_cache = if_match.map(|er| match_etag(&mut redis_conn, &cache_key, er.etag));
    match use_cache {
        None => Ok(()),
        Some(Ok(true)) => Ok(()),
        _ => Err(HttpError::PreconditionFailed.into()),
    }.map_err(|e: Error| e)?;

    let mut conn = db::get_conn(&server_state.db_pool)?;
    let rmq_publisher = &server_state.rmq_publisher;

    conn.build_transaction().run(|conn| -> Result<(), Error>{
        let agent = find_user(conn, subject.email)?;
        let agent = agent.ok_or(HttpError::Unauthorized)?;
        let agent = Agent::User(agent);
        let result = db::find_post(conn, id)?;
        let result = result.ok_or(HttpError::NotFound)?;

        let result = domain::usecases::edit_post(&agent, &result, data.into_inner().into());

        use domain::usecases::EditPostResult::*;
        match result {
            CantEditAnotherOnesPost => Err(HttpError::Forbidden.into()),
            CantEditPublishedPost => Err(HttpError::Conflict.into()),
            CantEditAsReader => Err(HttpError::Forbidden.into()),
            CantEditAsWorker => Err(HttpError::Forbidden.into()),
            NothingToUpdate => Ok(()),
            DoUpdate(post_id, post_edition) => {
                db::update_post(conn, post_id, post_edition)?;
                let cache_keys = ["posts".to_string(), format!("posts.{}", id).to_string()];
                for cache_key in cache_keys {
                    redis::refresh_etag(&mut redis_conn, &cache_key)?;
                }
                Ok(())
            }
            DoUpdateAndNotify(post_id, post_edition, post) => {
                db::update_post(conn, post_id, post_edition)?;

                let _ = rmqpub::notify_post_updated(&rmq_publisher, &post);

                let cache_keys = ["posts".to_string(), format!("posts.{}", id).to_string()];
                for cache_key in cache_keys {
                    redis::refresh_etag(&mut redis_conn, &cache_key)?;
                }
                Ok(())
            }
        }
    })?;

    Ok(NoContent)
}
