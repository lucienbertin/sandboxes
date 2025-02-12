use application::DeletePostResult;
use super::models::{NewPost, Post};
use rocket::{form::Form, http::Status, response::status::{Created, NoContent}, serde::json::Json};
use super::auth::JwtIdentifiedSubject;

#[get("/posts", format = "json", )]
pub fn get_posts(subject: JwtIdentifiedSubject) -> Result<Json<Vec<Post>>, rocket::response::status::Custom<&'static str>> {
    use adapters::select_published_posts;

    println!("{:?}", subject);

    let results = select_published_posts().map_err(|_e| rocket::response::status::Custom(Status::InternalServerError, "error"))?;
    let results = results.into_iter().map(|x| x.into()).collect();

    Ok(Json(results))
}

#[get("/post/<id>", format = "json")]
pub fn get_post(id: i32) -> Result<Json<Post>, rocket::response::status::Custom<&'static str>> {
    use adapters::find_post;

    let result = find_post(id).map_err(|_e| rocket::response::status::Custom(Status::InternalServerError, "error"))?;

    match result {
        Some(p) => Ok(Json(p.into())),
        None => Err(rocket::response::status::Custom(Status::NotFound, "not found"))
    }
}

#[post("/posts", data = "<post>")]
pub fn post_post(post: Form<NewPost>) -> Result<Created<Json<Post>>, rocket::response::status::Custom<&'static str>> {
    use adapters::insert_new_post;

    let result = insert_new_post(post.into_inner().into()).map_err(|_e| rocket::response::status::Custom(Status::InternalServerError, "error"))?;
    let location = format!("/api/post/{}", result.id);

    Ok(Created::new(location))
}

#[delete("/post/<id>")]
pub fn delete_post(subject: JwtIdentifiedSubject, id: i32) -> Result<NoContent, rocket::response::status::Custom<&'static str>> {
    use adapters::{find_post, delete_post};

    // imperative shell
    let post = find_post(id).map_err(|_e| rocket::response::status::Custom(Status::InternalServerError, "error"))?;

    //functional core
    let result = post.map(|p| application::delete_post(subject.email, p));

    // imperative shell
    match result {
        Some(DeletePostResult::DoDelete(post_id)) =>  delete_post(post_id).map_err(|_e| rocket::response::status::Custom(Status::InternalServerError, "error")),
        Some(DeletePostResult::CantDeleteAnotherOnesPost) => Err(rocket::response::status::Custom(Status::Forbidden, "cant delete another one's post")),
        Some(DeletePostResult::CantDeletePublishedPost) => Err(rocket::response::status::Custom(Status::Conflict, "cant delete published post")),
        None => Err(rocket::response::status::Custom(Status::Gone, "gone")),
    }?;

    Ok(NoContent)
}