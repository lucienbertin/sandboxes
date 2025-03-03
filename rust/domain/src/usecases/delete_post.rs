use crate::models::{Post, User, Role};

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
        Role::Writer if subject.email != post.author => DeletePostResult::CantDeleteAnotherOnesPost,
        Role::Writer if post.published => DeletePostResult::CantDeletePublishedPost,
        Role::Writer  => DeletePostResult::DoDelete(post.id),
        Role::Admin => DeletePostResult::DoDelete(post.id),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn as_reader() {
        // arrange
        let subject = User { id: 1, first_name: "test".to_string(), last_name: "test".to_string(), email: "test@te.st".to_string(), role: Role::Reader };
        let id = 1i32;
        let my_unpublished_post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
            author: subject.email.clone(),
        };
        let my_published_post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: true,
            author: subject.email.clone(),
        };
        let someone_elses_post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
            author: "someone@el.se".to_string(),
        };

        // act
        let result_my_unpublished_post = delete_post(&subject, &my_unpublished_post);
        let result_my_published_post = delete_post(&subject, &my_published_post);
        let result_someone_elses_post = delete_post(&subject, &someone_elses_post);

        // assert
        assert_eq!(result_my_unpublished_post, DeletePostResult::CantDeleteAsReader);
        assert_eq!(result_my_published_post, DeletePostResult::CantDeleteAsReader);
        assert_eq!(result_someone_elses_post, DeletePostResult::CantDeleteAsReader);
    }

    #[test]
    fn as_writer() {
        // arrange
        let subject = User { id: 1, first_name: "test".to_string(), last_name: "test".to_string(), email: "test@te.st".to_string(), role: Role::Writer };
        let id = 1i32;
        let my_unpublished_post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
            author: subject.email.clone(),
        };
        let my_published_post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: true,
            author: subject.email.clone(),
        };
        let someone_elses_post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
            author: "someone@el.se".to_string(),
        };

        // act
        let result_my_unpublished_post = delete_post(&subject, &my_unpublished_post);
        let result_my_published_post = delete_post(&subject, &my_published_post);
        let result_someone_elses_post = delete_post(&subject, &someone_elses_post);

        // assert
        assert_eq!(result_my_unpublished_post, DeletePostResult::DoDelete(id));
        assert_eq!(result_my_published_post, DeletePostResult::CantDeletePublishedPost);
        assert_eq!(result_someone_elses_post, DeletePostResult::CantDeleteAnotherOnesPost);
    }
    #[test]
    fn as_admin() {
        // arrange
        let subject = User { id: 1, first_name: "test".to_string(), last_name: "test".to_string(), email: "test@te.st".to_string(), role: Role::Admin };
        let id = 1i32;
        let my_unpublished_post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
            author: subject.email.clone(),
        };
        let my_published_post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: true,
            author: subject.email.clone(),
        };
        let someone_elses_post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
            author: "someone@el.se".to_string(),
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
