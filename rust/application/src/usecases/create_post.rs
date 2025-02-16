use crate::models::{NewPost, NewPostRequest};

pub enum CreatePostResult {
    DoCreate(NewPost),
}

pub fn create_post(subject: String, create_post_request: NewPostRequest) -> CreatePostResult {
    match subject.as_str() {
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