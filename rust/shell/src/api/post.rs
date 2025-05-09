use super::error::ResponseError;
use super::{check_match, check_none_match, get_etag_safe, AgentWrapper, DbConnWrapper, RedisConnWrapper};
use crate::db::{self};
use crate::error::{Error, HttpError};
use crate::redis::{self, RedisConn};
use crate::rmqpub::{self, RmQPublisher};
use diesel::PgConnection;
use domain::models::PostEdition;
use rocket::serde::{Deserialize, Serialize};
use rocket::{
    form::Form,
    http::Status,
    response::status::{Created, NoContent},
    serde::json::Json,
};

use super::{EtagJson, IfMatchHeader, IfNoneMatchHeader};

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
    mut db_conn: DbConnWrapper,
    mut redis_conn: RedisConnWrapper,
    agent: AgentWrapper,
    if_none_match: Option<IfNoneMatchHeader>,
) -> Result<EtagJson<Vec<Post>>, ResponseError> {
    let cache_key = "posts".to_string();
    check_none_match(&mut redis_conn, &cache_key, if_none_match)?;

    let posts = db_conn.build_transaction().read_only().run(
        |conn| -> Result<Vec<domain::models::Post>, Error> {
            let result = domain::usecases::consult_posts(&agent);

            use domain::usecases::ConsultPostsResult::*;
            match result {
                ConsultPublishedPosts => db::select_published_posts(conn),
                ConsultPublishedPostsAndAuthoredBy(author) => {
                    db::select_published_posts_or_authored_by(conn, author)
                }
                ConsultAllPosts => db::select_posts(conn),
            }
        },
    )?;

    let posts = posts.into_iter().map(|x| x.into()).collect();
    let etag = get_etag_safe(&mut redis_conn, &cache_key);
    let result = EtagJson::new(posts, etag);

    Ok(result)
}

#[get("/post/<id>", format = "json")]
pub fn get_post(
    mut db_conn: DbConnWrapper,
    mut redis_conn: RedisConnWrapper,
    agent: AgentWrapper,
    id: i32,
    if_none_match: Option<IfNoneMatchHeader>,
) -> Result<EtagJson<Post>, ResponseError> {
    let cache_key = format!("posts.{}", id).to_string();
    check_none_match(&mut redis_conn, &cache_key, if_none_match)?;


    let result = db_conn.build_transaction().read_only().run(
        |conn| -> Result<domain::models::Post, Error> {
            let post = db::find_post(conn, id)?;
            let post = post.ok_or(HttpError::NotFound)?;

            let result = domain::usecases::consult_post(&agent, &post);

            use domain::usecases::ConsultPostResult::*;
            match result {
                DoConsultPost(p) => Ok(p),
                CantConsultUnpublishedPostFromSomeoneElse => Err(HttpError::Forbidden.into()),
                CantConsultUnpublishedPostAsReader => Err(HttpError::Forbidden.into()),
                CantConsultUnpublishedPostAsUnknown => Err(HttpError::Unauthorized.into()),
            }
        },
    )?;

    let etag = match redis::get_etag(&mut redis_conn, &cache_key) {
        Ok(t) => Some(t),
        _ => None, // i dont want to 500 on a redis error when i have the results available
    };
    let result = EtagJson::new(result.into(), etag);

    Ok(result)
}

#[post("/post/<id>/publish")]
pub async fn publish_post(
    mut db_conn: DbConnWrapper,
    mut redis_conn: RedisConnWrapper,
    rmq_publisher: &RmQPublisher,
    agent: AgentWrapper,
    id: i32,
    if_match: Option<IfMatchHeader>,
) -> Result<Status, ResponseError> {
    let cache_key = format!("posts.{}", id).to_string();
    check_match(&mut redis_conn, &cache_key, if_match)?;

    db_conn.build_transaction()
        .run(move |conn| -> Result<(), Error> {
            let post = db::find_post(conn, id)?;
            let post = post.ok_or(HttpError::NotFound)?;

            let result = domain::usecases::publish_post(&agent, &post);

            let do_publish_and_notify = |db_conn: &mut PgConnection,
                                         rmq_publisher: &rmqpub::RmQPublisher,
                                         redis_conn: &mut RedisConn,
                                         post_id: i32,
                                         post: &domain::models::Post|
             -> Result<(), Error> {
                db::publish_post(db_conn, post_id)?;
                rmqpub::notify_post_published(&rmq_publisher, &post)?;

                let cache_keys = ["posts".to_string(), format!("posts.{}", id).to_string()];
                for cache_key in cache_keys {
                    redis::refresh_etag(redis_conn, &cache_key)?;
                }

                Ok(())
            };
            use domain::usecases::PublishPostResult::*;
            match result {
                CantPublishAnotherOnesPost => Err(HttpError::Forbidden.into()),
                CantPublishAsUnknown => Err(HttpError::Unauthorized.into()),
                CantPublishAsReader => Err(HttpError::Forbidden.into()),
                CantPublishAsWorker => Err(HttpError::Forbidden.into()), // should never happen
                CantPublishAlreadyPublishedPost => Ok(()), // treat it as Ok for idempotency purpose
                DoPublishAndNotify(post_id, post) => {
                    do_publish_and_notify(conn, rmq_publisher, &mut redis_conn, post_id, &post)
                }
                DoPublishNotifyAndSendMailToAuthor(post_id, post, author) => {
                    do_publish_and_notify(conn, rmq_publisher, &mut redis_conn, post_id, &post)?;
                    rmqpub::trigger_mail_post_published(&rmq_publisher, &post, &author)?;

                    Ok(())
                }
            }
        })?;

    Ok(Status::NoContent)
}

#[post("/posts", data = "<data>")]
pub fn post_post(
    mut db_conn: DbConnWrapper,
    mut redis_conn: RedisConnWrapper,
    agent: AgentWrapper,
    if_match: Option<IfMatchHeader>,
    data: Form<NewPost>,
) -> Result<Created<Json<Post>>, ResponseError> {
    let cache_key = "posts".to_string();
    check_match(&mut redis_conn, &cache_key, if_match)?;

    let result = db_conn.build_transaction().run(|conn| -> Result<i32, Error> {
        let result = domain::usecases::create_post(&agent, data.into_inner().into());
        use domain::usecases::CreatePostResult::*;
        match result {
            DoCreate(new_post) => {
                let post_id = db::insert_new_post(conn, new_post)?;

                let cache_key = "posts".to_string();
                redis::refresh_etag(&mut redis_conn, &cache_key)?;

                Ok(post_id)
            }
            CantCreateAsReader | CantCreateAsWorker => Err(HttpError::Forbidden.into()),
            CantCreateAsUnknown => Err(HttpError::Unauthorized.into()),
        }
    })?;

    let result = format!("/api/post/{}", result);

    Ok(Created::new(result))
}

#[delete("/post/<id>")]
pub fn delete_post(
    mut db_conn: DbConnWrapper,
    mut redis_conn: RedisConnWrapper,
    rmq_publisher: &RmQPublisher,
    agent: AgentWrapper,
    if_match: Option<IfMatchHeader>,
    id: i32,
) -> Result<NoContent, ResponseError> {
    let cache_key = format!("posts.{}", id).to_string();
    check_match(&mut redis_conn, &cache_key, if_match)?;

    db_conn.build_transaction().run(|conn| -> Result<(), Error> {
        let result = db::find_post(conn, id)?;
        let result = result.ok_or(HttpError::Gone)?;

        let result = domain::usecases::delete_post(&agent, &result);

        let do_delete = |conn: &mut PgConnection,
                         redis_conn: &mut RedisConn,
                         post_id: i32|
         -> Result<(), Error> {
            db::delete_post(conn, post_id)?;

            let cache_keys = ["posts".to_string(), format!("posts.{}", id).to_string()];
            for cache_key in cache_keys {
                redis::refresh_etag(redis_conn, &cache_key)?;
            }

            Ok(())
        };

        use domain::usecases::DeletePostResult::*;
        match result {
            CantDeleteAnotherOnesPost => Err(HttpError::Forbidden.into()),
            CantDeletePublishedPost => Err(HttpError::Conflict.into()),
            CantDeleteAsUnknown => Err(HttpError::Unauthorized.into()),
            CantDeleteAsReader => Err(HttpError::Forbidden.into()),
            CantDeleteAsWorker => Err(HttpError::Forbidden.into()),
            DoDelete(post_id) => do_delete(conn, &mut redis_conn, post_id),
            DoDeleteAndNotify(post_id, post) => {
                do_delete(conn, &mut redis_conn, post_id)?;
                rmqpub::notify_post_deleted(&rmq_publisher, &post)?;

                Ok(())
            }
        }
    })?;

    Ok(NoContent)
}

#[patch("/post/<id>", data = "<data>")]
pub fn patch_post(
    mut db_conn: DbConnWrapper,
    mut redis_conn: RedisConnWrapper,
    rmq_publisher: &RmQPublisher,
    agent: AgentWrapper,
    if_match: Option<IfMatchHeader>,
    id: i32,
    data: Form<PatchPost>,
) -> Result<NoContent, ResponseError> {
    let cache_key = format!("posts.{}", id).to_string();
    check_match(&mut redis_conn, &cache_key, if_match)?;

    db_conn.build_transaction().run(|conn| -> Result<(), Error> {
        let result = db::find_post(conn, id)?;
        let result = result.ok_or(HttpError::NotFound)?;

        let result = domain::usecases::edit_post(&agent, &result, data.into_inner().into());

        let do_update = |db_conn: &mut PgConnection,
                         redis_conn: &mut RedisConn,
                         post_id: i32,
                         edits: PostEdition|
         -> Result<(), Error> {
            db::update_post(db_conn, post_id, edits)?;
            let cache_keys = ["posts".to_string(), format!("posts.{}", id).to_string()];
            for cache_key in cache_keys {
                redis::refresh_etag(redis_conn, &cache_key)?;
            }
            Ok(())
        };
        use domain::usecases::EditPostResult::*;
        match result {
            CantEditAnotherOnesPost => Err(HttpError::Forbidden.into()),
            CantEditPublishedPost => Err(HttpError::Conflict.into()),
            CantEditAsUnknown => Err(HttpError::Unauthorized.into()),
            CantEditAsReader => Err(HttpError::Forbidden.into()),
            CantEditAsWorker => Err(HttpError::Forbidden.into()),
            NothingToUpdate => Ok(()),
            DoUpdate(post_id, edits) => do_update(conn, &mut redis_conn, post_id, edits),
            DoUpdateAndNotify(post_id, edits, post) => {
                do_update(conn, &mut redis_conn, post_id, edits)?;
                rmqpub::notify_post_updated(&rmq_publisher, &post)?;
                Ok(())
            }
        }
    })?;

    Ok(NoContent)
}
