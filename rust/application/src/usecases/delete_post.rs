use crate::models::Post;

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
