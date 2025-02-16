use crate::models::{Post, PostEdition};

pub enum EditPostResult {
    DoNothing,
    DoUpdate(i32, PostEdition),
    CantEditAnotherOnesPost,
    CantEditPublishedPost,
}

pub fn edit_post(subject: String, post: Post, request: PostEdition) -> EditPostResult {
    let check_subject = match subject.as_str() {
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