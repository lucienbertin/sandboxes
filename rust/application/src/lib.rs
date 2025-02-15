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
    CantPublishAnotherOnesPost,
    CantPublishAlreadyPublishedPost,
    SubjectBlacklisted(String),
}

pub fn publish_post(subject: String, post: Post) -> PublishPostResult {
    match subject.as_str() {
        "john@d.oe" => PublishPostResult::SubjectBlacklisted(subject),
        _s if post.published => PublishPostResult::CantPublishAlreadyPublishedPost,
        s if s != &post.author => PublishPostResult::CantPublishAnotherOnesPost,
        _ => PublishPostResult::DoPublish(post.id),
    }
}

pub enum EditPostResult {
    DoNothing,
    DoUpdate(i32, PostEdition),
    CantEditAnotherOnesPost,
    CantEditPublishedPost,
    SubjectBlacklisted(String),
}

pub fn edit_post(subject: String, post: Post, request: PostEdition) -> EditPostResult {
    let check_subject = match subject.as_str() {
        "john@d.oe" => Some(EditPostResult::SubjectBlacklisted(subject)),
        s if s != &post.author => Some(EditPostResult::CantEditAnotherOnesPost),
        _ => None
    };

    let check_post = match post.published {
        true => Some(EditPostResult::CantEditPublishedPost),
        false => None,
    };

    let edits = PostEdition{ 
        title: request.title.filter(|t| *t != post.title),
        body: request.body.filter(|b| *b != post.body),
    };
    let check_edits = match edits {
        PostEdition{ title: None, body: None } => Some(EditPostResult::DoNothing),
        _ => None
    };

    let result = check_subject
        .or(check_post)
        .or(check_edits)
        .or(Some(EditPostResult::DoUpdate(post.id, edits)))
        .unwrap();

    result
}