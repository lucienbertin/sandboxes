pub mod models;

use self::models::*;

pub enum DeletePostResult {
    DoDelete(i32),
    CantDeletePublishedPost,
    CantDeleteAnotherOnesPost
}
pub fn delete_post(subject: String, post: Post) -> DeletePostResult {
    if subject != post.author {
        return DeletePostResult::CantDeletePublishedPost; 
    }
    match post.published {
        true => DeletePostResult::CantDeletePublishedPost,
        false => DeletePostResult::DoDelete(post.id),
    }
}

pub enum CreatePostResult {
    DoCreate(NewPost),
    SubjectBlacklisted(String),
}

pub fn create_post(subject: String, create_post_request: NewPostRequest) -> CreatePostResult {
    match subject.as_str() {
        "john@d.oe" => CreatePostResult::SubjectBlacklisted(subject),
        s => {
            let new_post = NewPost{ 
                title: create_post_request.title,
                body: create_post_request.body,
                author: s.to_string(),
            };
            CreatePostResult::DoCreate(new_post)
        }
    }
}

pub enum PublishPostResult {
    DoPublish(i32),
    WrongAuthor,
    AlreadyPublished,
    SubjectBlacklisted(String),
}

pub fn publish_post(subject: String, post: Post) -> PublishPostResult {
    match subject.as_str() {
        "john@d.oe" => PublishPostResult::SubjectBlacklisted(subject),
        _s if post.published => PublishPostResult::AlreadyPublished,
        s if s != &post.author => PublishPostResult::WrongAuthor,
        _ => PublishPostResult::DoPublish(post.id),
    }
}