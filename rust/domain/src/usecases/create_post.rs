use crate::models::{NewPost, NewPostRequest};

#[derive(PartialEq, Debug)]
pub enum CreatePostResult {
    DoCreate(NewPost),
}

pub fn create_post(subject: &String, create_post_request: NewPostRequest) -> CreatePostResult {
    match subject.as_str() {
        s => {
            let new_post = NewPost {
                title: create_post_request.title,
                body: create_post_request.body,
                author: s.to_string(),
            };
            CreatePostResult::DoCreate(new_post)
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn happy_path() {
        // arrange
        let subject = "test@te.st".to_string();
        let request = NewPostRequest {
            title: "test".to_string(),
            body: "test".to_string(),
        };

        // act
        let result = create_post(&subject, request);

        // assert
        assert!(matches!(result, CreatePostResult::DoCreate(_)));
        let CreatePostResult::DoCreate(new_post) = result;
        assert_eq!(new_post.author, subject);
    }
}
