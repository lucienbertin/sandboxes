use crate::models::{NewPost, NewPostRequest, Role, User};

#[derive(PartialEq, Debug)]
pub enum CreatePostResult {
    DoCreate(NewPost),
    CantCreateAsReader,
}

pub fn create_post(subject: &User, create_post_request: NewPostRequest) -> CreatePostResult {
    match subject.role {
        Role::Admin | Role::Writer => {
            let new_post = NewPost {
                title: create_post_request.title,
                body: create_post_request.body,
                author_id: subject.id,
            };
            CreatePostResult::DoCreate(new_post)
        }
        Role::Reader => CreatePostResult::CantCreateAsReader,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn reader_cant_create_post() {
        // arrange
        let subject = User {
            id: 1,
            first_name: "test".to_string(),
            last_name: "test".to_string(),
            email: "test@te.st".to_string(),
            role: Role::Reader,
        };
        let request = NewPostRequest {
            title: "test".to_string(),
            body: "test".to_string(),
        };

        // act
        let result = create_post(&subject, request);

        // assert
        assert!(matches!(result, CreatePostResult::CantCreateAsReader));
    }

    #[test]
    fn happy_path_writer() {
        // arrange
        let subject = User {
            id: 1,
            first_name: "test".to_string(),
            last_name: "test".to_string(),
            email: "test@te.st".to_string(),
            role: Role::Writer,
        };
        let request = NewPostRequest {
            title: "test".to_string(),
            body: "test".to_string(),
        };

        // act
        let result = create_post(&subject, request);

        // assert
        assert!(matches!(result, CreatePostResult::DoCreate(_)));
        if let CreatePostResult::DoCreate(new_post) = result {
            assert_eq!(new_post.author_id, subject.id);
        }
    }

    #[test]
    fn happy_path_admin() {
        // arrange
        let subject = User {
            id: 1,
            first_name: "test".to_string(),
            last_name: "test".to_string(),
            email: "test@te.st".to_string(),
            role: Role::Admin,
        };
        let request = NewPostRequest {
            title: "test".to_string(),
            body: "test".to_string(),
        };

        // act
        let result = create_post(&subject, request);

        // assert
        assert!(matches!(result, CreatePostResult::DoCreate(_)));
        if let CreatePostResult::DoCreate(new_post) = result {
            assert_eq!(new_post.author_id, subject.id);
        }
    }
}
