use crate::models::{Post, Role, User};

#[derive(PartialEq, Debug)]
pub enum DeletePostResult {
    DoDelete(i32),
    CantDeleteAsReader,
    CantDeletePublishedPost,
    CantDeleteAnotherOnesPost,
}
pub fn delete_post(subject: &User, post: &Post) -> DeletePostResult {
    match subject.role {
        Role::Reader => DeletePostResult::CantDeleteAsReader,
        Role::Writer if subject.id != post.author_id => DeletePostResult::CantDeleteAnotherOnesPost,
        Role::Writer if post.published => DeletePostResult::CantDeletePublishedPost,
        Role::Writer => DeletePostResult::DoDelete(post.id),
        Role::Admin => DeletePostResult::DoDelete(post.id),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn as_reader() {
        // arrange
        let subject = User {
            id: 1,
            first_name: "test".to_string(),
            last_name: "test".to_string(),
            email: "test@te.st".to_string(),
            role: Role::Reader,
        };
        let id = 1i32;
        let my_unpublished_post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
            author_id: 1,
        };
        let my_published_post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: true,
            author_id: 1,
        };
        let someone_elses_post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
            author_id: 2,
        };

        // act
        let result_my_unpublished_post = delete_post(&subject, &my_unpublished_post);
        let result_my_published_post = delete_post(&subject, &my_published_post);
        let result_someone_elses_post = delete_post(&subject, &someone_elses_post);

        // assert
        assert_eq!(
            result_my_unpublished_post,
            DeletePostResult::CantDeleteAsReader
        );
        assert_eq!(
            result_my_published_post,
            DeletePostResult::CantDeleteAsReader
        );
        assert_eq!(
            result_someone_elses_post,
            DeletePostResult::CantDeleteAsReader
        );
    }

    #[test]
    fn as_writer() {
        // arrange
        let subject = User {
            id: 1,
            first_name: "test".to_string(),
            last_name: "test".to_string(),
            email: "test@te.st".to_string(),
            role: Role::Writer,
        };
        let id = 1i32;
        let my_unpublished_post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
            author_id: 1,
        };
        let my_published_post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: true,
            author_id: 1,
        };
        let someone_elses_post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
            author_id: 2,
        };

        // act
        let result_my_unpublished_post = delete_post(&subject, &my_unpublished_post);
        let result_my_published_post = delete_post(&subject, &my_published_post);
        let result_someone_elses_post = delete_post(&subject, &someone_elses_post);

        // assert
        assert_eq!(result_my_unpublished_post, DeletePostResult::DoDelete(id));
        assert_eq!(
            result_my_published_post,
            DeletePostResult::CantDeletePublishedPost
        );
        assert_eq!(
            result_someone_elses_post,
            DeletePostResult::CantDeleteAnotherOnesPost
        );
    }
    #[test]
    fn as_admin() {
        // arrange
        let subject = User {
            id: 1,
            first_name: "test".to_string(),
            last_name: "test".to_string(),
            email: "test@te.st".to_string(),
            role: Role::Admin,
        };
        let id = 1i32;
        let my_unpublished_post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
            author_id: 1,
        };
        let my_published_post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: true,
            author_id: 1,
        };
        let someone_elses_post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
            author_id: 2,
        };

        // act
        let result_my_unpublished_post = delete_post(&subject, &my_unpublished_post);
        let result_my_published_post = delete_post(&subject, &my_published_post);
        let result_someone_elses_post = delete_post(&subject, &someone_elses_post);

        // assert
        assert_eq!(result_my_unpublished_post, DeletePostResult::DoDelete(id));
        assert_eq!(result_my_published_post, DeletePostResult::DoDelete(id));
        assert_eq!(result_someone_elses_post, DeletePostResult::DoDelete(id));
    }
}
