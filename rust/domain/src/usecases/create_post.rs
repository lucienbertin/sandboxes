use crate::models::{Agent, NewPost, NewPostRequest, Role, User};

#[derive(PartialEq, Debug)]
pub enum CreatePostResult {
    DoCreate(NewPost),
    CantCreateAsReader,
    CantCreateAsWorker,
    CantCreateAsUnknown,
}

pub fn create_post(agent: &Agent, create_post_request: NewPostRequest) -> CreatePostResult {
    use CreatePostResult::*;
    use Role::*;
    match agent {
        Agent::Unknown => CantCreateAsUnknown,
        Agent::Worker => CantCreateAsWorker,
        Agent::User(User { role: Reader, .. }) => CantCreateAsReader,
        Agent::User(u) => {
            let new_post = NewPost {
                title: create_post_request.title,
                body: create_post_request.body,
                author: u.clone(),
            };
            CreatePostResult::DoCreate(new_post)
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn reader_cant_create_post() {
        // arrange
        let reader = User {
            id: 1,
            first_name: "test".to_string(),
            last_name: "test".to_string(),
            email: "test@te.st".to_string(),
            role: Role::Reader,
        };
        let agent = Agent::User(reader);
        let request = NewPostRequest {
            title: "test".to_string(),
            body: "test".to_string(),
        };

        // act
        let result = create_post(&agent, request);

        // assert
        assert!(matches!(result, CreatePostResult::CantCreateAsReader));
    }

    #[test]
    fn happy_path_writer() {
        // arrange
        let writer = User {
            id: 1,
            first_name: "test".to_string(),
            last_name: "test".to_string(),
            email: "test@te.st".to_string(),
            role: Role::Writer,
        };
        let agent = Agent::User(writer.clone());
        let request = NewPostRequest {
            title: "test".to_string(),
            body: "test".to_string(),
        };

        // act
        let result = create_post(&agent, request);

        // assert
        assert!(matches!(result, CreatePostResult::DoCreate(_)));
        if let CreatePostResult::DoCreate(new_post) = result {
            assert_eq!(new_post.author.id, writer.id);
        }
    }

    #[test]
    fn happy_path_admin() {
        // arrange
        let admin = User {
            id: 1,
            first_name: "test".to_string(),
            last_name: "test".to_string(),
            email: "test@te.st".to_string(),
            role: Role::Admin,
        };
        let agent = Agent::User(admin.clone());
        let request = NewPostRequest {
            title: "test".to_string(),
            body: "test".to_string(),
        };

        // act
        let result = create_post(&agent, request);

        // assert
        assert!(matches!(result, CreatePostResult::DoCreate(_)));
        if let CreatePostResult::DoCreate(new_post) = result {
            assert_eq!(new_post.author.id, admin.id);
        }
    }
}
