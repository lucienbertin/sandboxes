use crate::models::{Agent, Post, User};

#[derive(PartialEq, Debug)]
pub enum PublishPostResult {
    DoPublishAndNotify(i32, Post),
    DoPublishNotifyAndSendMailToAuthor(i32, Post, User),
    CantPublishAnotherOnesPost,
    CantPublishAlreadyPublishedPost,
    CantPublishAsReader,
    CantPublishAsWorker,
}

pub fn publish_post(agent: &Agent, post: &Post) -> PublishPostResult {
    use PublishPostResult::*;
    use crate::models::Role::*;
    match (agent, post) {
        (Agent::Worker, _) => CantPublishAsWorker,
        (Agent::User(User { role: Reader, .. }), _) => CantPublishAsReader,
        (Agent::User(User { role: Admin, .. }), Post { published: true, .. }) => CantPublishAlreadyPublishedPost,
        (Agent::User(User { role: Admin, id: admin_id, .. }), p) if p.author.id != *admin_id => DoPublishNotifyAndSendMailToAuthor(p.id, p.clone(), p.author.clone()),
        (Agent::User(User { role: Admin, .. }), p)  => DoPublishAndNotify(p.id, p.clone()),
        (Agent::User(writer), Post { author, .. }) if author != writer => CantPublishAnotherOnesPost,
        (_, Post { published: true, .. }) => CantPublishAlreadyPublishedPost,
        (_, p) => DoPublishAndNotify(p.id, p.clone()),
    }
}

#[cfg(test)]
mod test {
    use crate::models::Role;

    use super::*;

    #[test]
    fn as_worker() {
        // arrange
        let agent = Agent::Worker;
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
        let result = publish_post(&agent, &post);

        // assert
        assert_eq!(result, PublishPostResult::CantPublishAsWorker);
    }
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
        let agent = Agent::User(subject.clone());
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
        let result = publish_post(&agent, &post);

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
        let agent = Agent::User(subject.clone());
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
        let result = publish_post(&agent, &post);

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
        let agent = Agent::User(subject.clone());
        let post = Post {
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: true,
            author: subject.clone(),
        };

        // act
        let result = publish_post(&agent, &post);

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
        let agent = Agent::User(subject.clone());
        let id = 1i32;
        let post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
            author: subject.clone(),
        };

        // act
        let result = publish_post(&agent, &post);

        // assert
        assert_eq!(result, PublishPostResult::DoPublishAndNotify(id, post));
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
        let agent = Agent::User(subject.clone());
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
        let result_someone_elses_post = publish_post(&agent, &someone_elses_post);
        let result_my_unpublished_post = publish_post(&agent, &my_unpublished_post);
        let result_my_published_post = publish_post(&agent, &my_published_post);

        // assert
        assert_eq!(
            result_someone_elses_post,
            PublishPostResult::DoPublishNotifyAndSendMailToAuthor(
                id,
                someone_elses_post,
                someoneelse
            )
        );
        assert_eq!(
            result_my_unpublished_post,
            PublishPostResult::DoPublishAndNotify(id, my_unpublished_post)
        );
        assert_eq!(
            result_my_published_post,
            PublishPostResult::CantPublishAlreadyPublishedPost
        );
    }
}
