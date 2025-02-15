
use std::result;

use super::models::{NewPost, Post};
use rocket::{form::Form, http::Status, response::status::{Created, NoContent}, serde::json::Json};
use super::auth::JwtIdentifiedSubject;

#[get("/posts", format = "json", )]
pub fn get_posts(_subject: JwtIdentifiedSubject) -> Result<Json<Vec<Post>>, rocket::response::status::Custom<String>> {
    use adapters::select_published_posts;

    let results = select_published_posts().map_err(|e| rocket::response::status::Custom(Status::InternalServerError, format!("error: {:?}", e).to_string()))?;
    let results = results.into_iter().map(|x| x.into()).collect();

    Ok(Json(results))
}

#[get("/post/<id>", format = "json")]
pub fn get_post(id: i32) -> Result<Json<Post>, rocket::response::status::Custom<String>> {
    use adapters::find_post;

    let result = find_post(id).map_err(|e| rocket::response::status::Custom(Status::InternalServerError, format!("error: {:?}", e).to_string()))?;

    match result {
        Some(p) => Ok(Json(p.into())),
        None => Err(rocket::response::status::Custom(Status::NotFound, "not found".to_string()))
    }
}

#[post("/post/<id>/publish")]
pub fn publish_post(subject: JwtIdentifiedSubject, id: i32) -> Result<Status, rocket::response::status::Custom<String>> {
    use adapters::find_post;
    use application::{publish_post, PublishPostResult};

    let result = find_post(id).map_err(|e| rocket::response::status::Custom(Status::InternalServerError, format!("error: {:?}", e).to_string()))?;
    let result = result.ok_or(rocket::response::status::Custom(Status::Gone, "gone".to_string()))?;

    let result = publish_post(subject.email, result);

    let result = match result {
        PublishPostResult::SubjectBlacklisted(sub) => Err(rocket::response::status::Custom(Status::Forbidden, format!("the user {} is not allowed to write posts anymore", sub).to_string())),
        PublishPostResult::WrongAuthor => Err(rocket::response::status::Custom(Status::Forbidden, "you are not allowed to publish this post".to_string())),
        PublishPostResult::AlreadyPublished => Ok(()), // treat it as Ok for idempotency purpose
        PublishPostResult::DoPublish(post_id) => adapters::publish_post(post_id).map_err(|e| rocket::response::status::Custom(Status::InternalServerError, format!("error: {:?}", e).to_string()))
    }?;

    Ok(Status::NoContent)
}

#[post("/posts", data = "<post>")]
pub fn post_post(subject: JwtIdentifiedSubject, post: Form<NewPost>) -> Result<Created<Json<Post>>, rocket::response::status::Custom<String>> {
    use application::{create_post, CreatePostResult};
    use adapters::insert_new_post;

    let result = create_post(subject.email, post.into_inner().into());

    let result = match result {
        CreatePostResult::DoCreate(new_post) => insert_new_post(new_post).map_err(|e| rocket::response::status::Custom(Status::InternalServerError, format!("error: {:?}", e).to_string())),
        CreatePostResult::SubjectBlacklisted(sub) => Err(rocket::response::status::Custom(Status::Forbidden, format!("the user {} is not allowed to write posts anymore", sub).to_string()))
    }?;

    let result = format!("/api/post/{}", result.id);

    Ok(Created::new(result))
}

#[delete("/post/<id>")]
pub fn delete_post(subject: JwtIdentifiedSubject, id: i32) -> Result<NoContent, rocket::response::status::Custom<String>> {
    use adapters::{find_post, delete_post};
    use application::DeletePostResult;
    // imperative shell
    let post = find_post(id).map_err(|e| rocket::response::status::Custom(Status::InternalServerError, format!("error: {:?}", e).to_string()))?;

    //functional core
    let result = post.map(|p| application::delete_post(subject.email, p));

    // imperative shell
    match result {
        Some(DeletePostResult::DoDelete(post_id)) =>  delete_post(post_id).map_err(|e| rocket::response::status::Custom(Status::InternalServerError, format!("error: {:?}", e).to_string())),
        Some(DeletePostResult::CantDeleteAnotherOnesPost) => Err(rocket::response::status::Custom(Status::Forbidden, "cant delete another one's post".to_string())),
        Some(DeletePostResult::CantDeletePublishedPost) => Err(rocket::response::status::Custom(Status::Conflict, "cant delete published post".to_string())),
        None => Err(rocket::response::status::Custom(Status::Gone, "gone".to_string())),
    }?;

    Ok(NoContent)
}