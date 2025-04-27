use crate::models::{Post, Role::*, User};

#[derive(PartialEq, Debug)]
pub enum PublishPostResult {
    DoPublish(i32),
    DoPublishAndNotifyAuthor(i32, User),
    CantPublishAnotherOnesPost,
    CantPublishAlreadyPublishedPost,
    CantPublishAsReader,
}

pub fn publish_post(subject: &User, post: &Post) -> PublishPostResult {
    match subject.role {
        Reader => PublishPostResult::CantPublishAsReader,
        Writer if subject.id != post.author.id => PublishPostResult::CantPublishAnotherOnesPost,
        _ if post.published => PublishPostResult::CantPublishAlreadyPublishedPost,
        Admin if subject.id != post.author.id => {
            PublishPostResult::DoPublishAndNotifyAuthor(post.id, post.author.clone())
        }
        _ => PublishPostResult::DoPublish(post.id),
    }
}

#[cfg(test)]
mod test {
    use crate::models::Role;

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
        let someoneelse = User {
            id: 2,
            first_name: "someone".to_string(),
            last_name: "else".to_string(),
            email: "someone@el.se".to_string(),
            role: Role::Writer,
        };
        let post = Post {
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
            author: someoneelse.clone(),
        };

        // act
        let result = publish_post(&subject, &post);

        // assert
        assert_eq!(result, PublishPostResult::CantPublishAsReader);
    }

    #[test]
    fn wrong_author() {
        // arrange
        let subject = User {
            id: 1,
            first_name: "test".to_string(),
            last_name: "test".to_string(),
            email: "test@te.st".to_string(),
            role: Role::Writer,
        };
        let someoneelse = User {
            id: 2,
            first_name: "someone".to_string(),
            last_name: "else".to_string(),
            email: "spmepne@el.se".to_string(),
            role: Role::Writer,
        };
        let post = Post {
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
            author: someoneelse.clone(),
        };

        // act
        let result = publish_post(&subject, &post);

        // assert
        assert_eq!(result, PublishPostResult::CantPublishAnotherOnesPost);
    }

    #[test]
    fn already_published() {
        // arrange
        let subject = User {
            id: 1,
            first_name: "test".to_string(),
            last_name: "test".to_string(),
            email: "test@te.st".to_string(),
            role: Role::Writer,
        };
        let post = Post {
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: true,
            author: subject.clone(),
        };

        // act
        let result = publish_post(&subject, &post);

        // assert
        assert_eq!(result, PublishPostResult::CantPublishAlreadyPublishedPost);
    }

    #[test]
    fn happy_path() {
        // arrange
        let subject = User {
            id: 1,
            first_name: "test".to_string(),
            last_name: "test".to_string(),
            email: "test@te.st".to_string(),
            role: Role::Writer,
        };
        let id = 1i32;
        let post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
            author: subject.clone(),
        };

        // act
        let result = publish_post(&subject, &post);

        // assert
        assert_eq!(result, PublishPostResult::DoPublish(id));
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
        let someoneelse = User {
            id: 2,
            first_name: "someone".to_string(),
            last_name: "else".to_string(),
            email: "spmepne@el.se".to_string(),
            role: Role::Writer,
        };
        let id = 1i32;
        let someone_elses_post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
            author: someoneelse.clone(),
        };
        let my_unpublished_post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
            author: subject.clone(),
        };
        let my_published_post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: true,
            author: subject.clone(),
        };

        // act
        let result_someone_elses_post = publish_post(&subject, &someone_elses_post);
        let result_my_unpublished_post = publish_post(&subject, &my_unpublished_post);
        let result_my_published_post = publish_post(&subject, &my_published_post);

        // assert
        assert_eq!(
            result_someone_elses_post,
            PublishPostResult::DoPublishAndNotifyAuthor(id, someoneelse)
        );
        assert_eq!(result_my_unpublished_post, PublishPostResult::DoPublish(id));
        assert_eq!(
            result_my_published_post,
            PublishPostResult::CantPublishAlreadyPublishedPost
        );
    }
}
