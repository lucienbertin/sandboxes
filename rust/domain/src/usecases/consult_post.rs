use crate::models::{Post, Role, User, Agent};

#[derive(PartialEq, Debug)]
pub enum ConsultPostResult {
    DoConsultPost(Post),
    CantConsultUnpublishedPostFromSomeoneElse,
    CantConsultUnpublishedPostAsReader,
}
pub fn consult_post(agent: &Agent, post: &Post) -> ConsultPostResult {
    use self::ConsultPostResult::*;
    match (agent, post) {
        (_, Post { published: true, .. })                                => DoConsultPost(post.clone()),
        (Agent::Worker, _)                                               => DoConsultPost(post.clone()),
        (Agent::User(User { role: Role::Admin, .. }), _)                 => DoConsultPost(post.clone()),
        (Agent::User(u), Post { author: a, .. }) if u == a => DoConsultPost(post.clone()),
        (Agent::User(User { role: Role::Reader, .. }), _)                => CantConsultUnpublishedPostAsReader,
        _                                                                => CantConsultUnpublishedPostFromSomeoneElse,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn as_reader() {
        // arrange
        let agent = Agent::User(User {
            id: 1,
            first_name: "test".to_string(),
            last_name: "test".to_string(),
            email: "test@te.st".to_string(),
            role: Role::Reader,
        });
        let someone_else = User {
            id: 1,
            first_name: "someone".to_string(),
            last_name: "else".to_string(),
            email: "someone@el.se".to_string(),
            role: Role::Writer,
        };
        let unpublished_post = Post {
            author: someone_else.clone(),
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
        };
        let published_post = Post {
            author: someone_else.clone(),
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: true,
        };

        // act
        let result_unpublished = consult_post(&agent, &unpublished_post);
        let result_published = consult_post(&agent, &published_post);

        // assert
        assert!(matches!(
            result_unpublished,
            ConsultPostResult::CantConsultUnpublishedPostAsReader
        ));
        assert!(matches!(result_published, ConsultPostResult::DoConsultPost(_)));
    }

    #[test]
    fn as_writer() {
        // arrange
        let email = "test@te.st".to_string();
        let writer = User {
            id: 1,
            first_name: "test".to_string(),
            last_name: "test".to_string(),
            email: email.clone(),
            role: Role::Writer,
        };
        let agent = Agent::User(writer.clone());
        let someone_else = User {
            id: 2,
            first_name: "someone".to_string(),
            last_name: "else".to_string(),
            email: "someone@el.se".to_string(),
            role: Role::Writer,
        };
        let my_unpublished_post = Post {
            author: writer.clone(),
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
        };
        let someone_elses_unpublished_post = Post {
            author: someone_else.clone(),
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
        };
        let published_post = Post {
            author: someone_else.clone(),
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: true,
        };

        // act
        let result_my_unpublished_post = consult_post(&agent, &my_unpublished_post);
        let result_someone_elses_unpublished_post =
            consult_post(&agent, &someone_elses_unpublished_post);
        let result_published = consult_post(&agent, &published_post);

        // assert
        assert!(matches!(
            result_my_unpublished_post,
            ConsultPostResult::DoConsultPost(_)
        ));
        assert!(matches!(
            result_someone_elses_unpublished_post,
            ConsultPostResult::CantConsultUnpublishedPostFromSomeoneElse
        ));
        assert!(matches!(result_published, ConsultPostResult::DoConsultPost(_)));
    }

    #[test]
    fn as_admin() {
        // arrange
        let email = "test@te.st".to_string();
        let admin = User {
            id: 1,
            first_name: "test".to_string(),
            last_name: "test".to_string(),
            email: email.clone(),
            role: Role::Admin,
        };
        let agent = Agent::User(admin.clone());
        let someone_else = User {
            id: 1,
            first_name: "someone".to_string(),
            last_name: "else".to_string(),
            email: "someone@el.se".to_string(),
            role: Role::Writer,
        };
        let my_unpublished_post = Post {
            author: admin.clone(),
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
        };
        let someone_elses_unpublished_post = Post {
            author: someone_else.clone(),
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
        };
        let published_post = Post {
            author: someone_else.clone(),
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: true,
        };

        // act
        let result_my_unpublished_post = consult_post(&agent, &my_unpublished_post);
        let result_someone_elses_unpublished_post =
            consult_post(&agent, &someone_elses_unpublished_post);
        let result_published = consult_post(&agent, &published_post);

        // assert
        assert!(matches!(
            result_my_unpublished_post,
            ConsultPostResult::DoConsultPost(_)
        ));
        assert!(matches!(
            result_someone_elses_unpublished_post,
            ConsultPostResult::DoConsultPost(_)
        ));
        assert!(matches!(result_published, ConsultPostResult::DoConsultPost(_)));
    }
    #[test]
    fn as_worker() {
        // arrange
        let agent = Agent::Worker;
        let email = "test@te.st".to_string();
        let writer = User {
            id: 1,
            first_name: "test".to_string(),
            last_name: "test".to_string(),
            email: email.clone(),
            role: Role::Writer,
        };
        let someone_else = User {
            id: 2,
            first_name: "someone".to_string(),
            last_name: "else".to_string(),
            email: "someone@el.se".to_string(),
            role: Role::Writer,
        };
        let my_unpublished_post = Post {
            author: writer.clone(),
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
        };
        let someone_elses_unpublished_post = Post {
            author: someone_else.clone(),
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
        };
        let published_post = Post {
            author: someone_else.clone(),
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: true,
        };

        // act
        let result_my_unpublished_post = consult_post(&agent, &my_unpublished_post);
        let result_someone_elses_unpublished_post =
            consult_post(&agent, &someone_elses_unpublished_post);
        let result_published = consult_post(&agent, &published_post);

        // assert
        assert!(matches!(
            result_my_unpublished_post,
            ConsultPostResult::DoConsultPost(_)
        ));
        assert!(matches!(
            result_someone_elses_unpublished_post,
            ConsultPostResult::DoConsultPost(_)
        ));
        assert!(matches!(result_published, ConsultPostResult::DoConsultPost(_)));
    }
}
