use crate::models::{Post, Role, User};

#[derive(PartialEq, Debug)]
pub enum ConsultPostResult {
    DoConsultPost,
    CantConsultUnpublishedPostFromSomeoneElse,
    CantConsultUnpublishedPostAsReader,
}
pub fn consult_post(subject: &User, post: &Post) -> ConsultPostResult {
    match subject.role {
        Role::Reader if post.published => ConsultPostResult::DoConsultPost,
        Role::Reader => ConsultPostResult::CantConsultUnpublishedPostAsReader,
        Role::Writer if subject.id != post.author.id && !post.published => {
            ConsultPostResult::CantConsultUnpublishedPostFromSomeoneElse
        }
        Role::Writer => ConsultPostResult::DoConsultPost,
        Role::Admin => ConsultPostResult::DoConsultPost,
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
        let result_unpublished = consult_post(&subject, &unpublished_post);
        let result_published = consult_post(&subject, &published_post);

        // assert
        assert!(matches!(
            result_unpublished,
            ConsultPostResult::CantConsultUnpublishedPostAsReader
        ));
        assert!(matches!(result_published, ConsultPostResult::DoConsultPost));
    }

    #[test]
    fn as_writer() {
        // arrange
        let email = "test@te.st".to_string();
        let subject = User {
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
            author: subject.clone(),
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
        let result_my_unpublished_post = consult_post(&subject, &my_unpublished_post);
        let result_someone_elses_unpublished_post =
            consult_post(&subject, &someone_elses_unpublished_post);
        let result_published = consult_post(&subject, &published_post);

        // assert
        assert!(matches!(
            result_my_unpublished_post,
            ConsultPostResult::DoConsultPost
        ));
        assert!(matches!(
            result_someone_elses_unpublished_post,
            ConsultPostResult::CantConsultUnpublishedPostFromSomeoneElse
        ));
        assert!(matches!(result_published, ConsultPostResult::DoConsultPost));
    }

    #[test]
    fn as_admin() {
        // arrange
        let email = "test@te.st".to_string();
        let subject = User {
            id: 1,
            first_name: "test".to_string(),
            last_name: "test".to_string(),
            email: email.clone(),
            role: Role::Admin,
        };
        let someone_else = User {
            id: 1,
            first_name: "someone".to_string(),
            last_name: "else".to_string(),
            email: "someone@el.se".to_string(),
            role: Role::Writer,
        };
        let my_unpublished_post = Post {
            author: subject.clone(),
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
        let result_my_unpublished_post = consult_post(&subject, &my_unpublished_post);
        let result_someone_elses_unpublished_post =
            consult_post(&subject, &someone_elses_unpublished_post);
        let result_published = consult_post(&subject, &published_post);

        // assert
        assert!(matches!(
            result_my_unpublished_post,
            ConsultPostResult::DoConsultPost
        ));
        assert!(matches!(
            result_someone_elses_unpublished_post,
            ConsultPostResult::DoConsultPost
        ));
        assert!(matches!(result_published, ConsultPostResult::DoConsultPost));
    }
}
