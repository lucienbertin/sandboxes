pub mod models;

use self::models::Post;

pub enum DeletePostResult {
    DoDelete(i32),
    CantDeletePublishedPost()
}
pub fn delete_post(post: Post) -> DeletePostResult {
    match post.published {
        false => DeletePostResult::DoDelete(post.id),
        true => DeletePostResult::CantDeletePublishedPost()
    }
}