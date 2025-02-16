use crate::models::Post;

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
