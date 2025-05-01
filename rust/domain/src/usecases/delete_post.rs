use crate::models::{Agent, Post, Role, User};

#[derive(PartialEq, Debug)]
pub enum DeletePostResult {
    DoDelete(i32),
    DoDeleteAndNotify(i32, Post),
    CantDeleteAsReader,
    CantDeleteAsWorker,
    CantDeletePublishedPost,
    CantDeleteAnotherOnesPost,
}
pub fn delete_post(agent: &Agent, post: &Post) -> DeletePostResult {
    use DeletePostResult::*;
    use Role::*;

    match (agent, post) {
        (Agent::Worker, _) => CantDeleteAsWorker,
        (Agent::User(User { role: Reader, .. }), _) => CantDeleteAsReader,
        (Agent::User(User { role: Admin, .. }), p) if p.published => {
            DoDeleteAndNotify(p.id, p.clone())
        }
        (Agent::User(User { role: Admin, .. }), p) => DoDelete(p.id),
        (Agent::User(writer), Post { author, .. }) if author != writer => CantDeleteAnotherOnesPost,
        (
            _,
            Post {
                published: true, ..
            },
        ) => CantDeletePublishedPost,
        (_, p) => DoDelete(p.id),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn as_worker() {
        let agent = Agent::Worker;
        let someoneelse = User {
            id: 1,
            first_name: "someone".to_string(),
            last_name: "else".to_string(),
            email: "spmepne@el.se".to_string(),
            role: Role::Writer,
        };
        let id = 1i32;
        let someone_elses_published_post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: true,
            author: someoneelse.clone(),
        };
        let someone_elses_post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
            author: someoneelse.clone(),
        };

        // act
        let result_someone_elses_published_post =
            delete_post(&agent, &someone_elses_published_post);
        let result_someone_elses_post = delete_post(&agent, &someone_elses_post);

        // assert
        assert_eq!(
            result_someone_elses_published_post,
            DeletePostResult::CantDeleteAsWorker
        );
        assert_eq!(
            result_someone_elses_post,
            DeletePostResult::CantDeleteAsWorker
        );
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
            id: 1,
            first_name: "someone".to_string(),
            last_name: "else".to_string(),
            email: "spmepne@el.se".to_string(),
            role: Role::Writer,
        };
        let id = 1i32;
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
        let someone_elses_post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
            author: someoneelse.clone(),
        };

        // act
        let result_my_unpublished_post = delete_post(&agent, &my_unpublished_post);
        let result_my_published_post = delete_post(&agent, &my_published_post);
        let result_someone_elses_post = delete_post(&agent, &someone_elses_post);

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
        let agent = Agent::User(subject.clone());
        let someoneelse = User {
            id: 2,
            first_name: "someone".to_string(),
            last_name: "else".to_string(),
            email: "spmepne@el.se".to_string(),
            role: Role::Writer,
        };
        let id = 1i32;
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
        let someone_elses_post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
            author: someoneelse.clone(),
        };

        // act
        let result_my_unpublished_post = delete_post(&agent, &my_unpublished_post);
        let result_my_published_post = delete_post(&agent, &my_published_post);
        let result_someone_elses_post = delete_post(&agent, &someone_elses_post);

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
        let agent = Agent::User(subject.clone());
        let someoneelse = User {
            id: 2,
            first_name: "someone".to_string(),
            last_name: "else".to_string(),
            email: "spmepne@el.se".to_string(),
            role: Role::Writer,
        };
        let id = 1i32;
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
        let someone_elses_post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
            author: someoneelse.clone(),
        };
        let someone_elses_published_post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: true,
            author: someoneelse.clone(),
        };

        // act
        let result_my_unpublished_post = delete_post(&agent, &my_unpublished_post);
        let result_my_published_post = delete_post(&agent, &my_published_post);
        let result_someone_elses_post = delete_post(&agent, &someone_elses_post);
        let result_someone_elses_published_post =
            delete_post(&agent, &someone_elses_published_post);

        // assert
        assert_eq!(result_my_unpublished_post, DeletePostResult::DoDelete(id));
        assert_eq!(
            result_my_published_post,
            DeletePostResult::DoDeleteAndNotify(id, my_published_post)
        );
        assert_eq!(result_someone_elses_post, DeletePostResult::DoDelete(id));
        assert_eq!(
            result_someone_elses_published_post,
            DeletePostResult::DoDeleteAndNotify(id, someone_elses_published_post)
        );
    }
}
