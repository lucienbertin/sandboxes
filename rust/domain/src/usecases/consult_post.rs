use crate::models::{Role, User, Post};


#[derive(PartialEq, Debug)]
pub enum ConsultPostResult {
    DoConsultPost,
    CantConsultUnpublishedPostFromSomeoneElse,
    CantConsultUnpublishedPostAsReader,
}
pub fn consult_post(subject: &Option<User>, post: &Post) -> ConsultPostResult {
    match subject {
        None if post.published => ConsultPostResult::DoConsultPost,
        None => ConsultPostResult::CantConsultUnpublishedPostAsReader,
        Some(s) if s.role == Role::Reader && post.published => ConsultPostResult::DoConsultPost,
        Some(s) if s.role == Role::Reader => ConsultPostResult::CantConsultUnpublishedPostAsReader,
        Some(s) if s.role == Role::Writer && s.email != post.author && !post.published => ConsultPostResult::CantConsultUnpublishedPostFromSomeoneElse,
        Some(s) if s.role == Role::Writer => ConsultPostResult::DoConsultPost,
        Some(s) if s.role == Role::Admin => ConsultPostResult::DoConsultPost,
        Some(_) => ConsultPostResult::DoConsultPost,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn as_anonymous() {
        // arrange
        let subject = None;
        let unpublished_post = Post {
            author: "someone@el.se".to_string(),
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
        };
        let published_post = Post {
            author: "someone@el.se".to_string(),
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: true,
        };

        // act
        let result_unpublished = consult_post(&subject, &unpublished_post);
        let result_published = consult_post(&subject, &published_post);

        // assert
        assert!(matches!(result_unpublished, ConsultPostResult::CantConsultUnpublishedPostAsReader));
        assert!(matches!(result_published, ConsultPostResult::DoConsultPost));
    }

    #[test]
    fn as_reader() {
        // arrange
        let subject = Some(User { id: 1, first_name: "test".to_string(), last_name: "test".to_string(), email: "test@te.st".to_string(), role: Role::Reader });
        let unpublished_post = Post {
            author: "someone@el.se".to_string(),
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
        };
        let published_post = Post {
            author: "someone@el.se".to_string(),
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: true,
        };

        // act
        let result_unpublished = consult_post(&subject, &unpublished_post);
        let result_published = consult_post(&subject, &published_post);

        // assert
        assert!(matches!(result_unpublished, ConsultPostResult::CantConsultUnpublishedPostAsReader));
        assert!(matches!(result_published, ConsultPostResult::DoConsultPost));
    }

    #[test]
    fn as_writer() {
        // arrange
        let email = "test@te.st".to_string();
        let subject = Some(User { id: 1, first_name: "test".to_string(), last_name: "test".to_string(), email: email.clone(), role: Role::Writer });
        let my_unpublished_post = Post {
            author: email.to_string(),
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
        };
        let someone_elses_unpublished_post = Post {
            author: "someone@el.se".to_string(),
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
        };
        let published_post = Post {
            author: "someone@el.se".to_string(),
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: true,
        };

        // act
        let result_my_unpublished_post = consult_post(&subject, &my_unpublished_post);
        let result_someone_elses_unpublished_post = consult_post(&subject, &someone_elses_unpublished_post);
        let result_published = consult_post(&subject, &published_post);

        // assert
        assert!(matches!(result_my_unpublished_post, ConsultPostResult::DoConsultPost));
        assert!(matches!(result_someone_elses_unpublished_post, ConsultPostResult::CantConsultUnpublishedPostFromSomeoneElse));
        assert!(matches!(result_published, ConsultPostResult::DoConsultPost));
    }

    #[test]
    fn as_admin() {
        // arrange
        let email = "test@te.st".to_string();
        let subject = Some(User { id: 1, first_name: "test".to_string(), last_name: "test".to_string(), email: email.clone(), role: Role::Admin });
        let my_unpublished_post = Post {
            author: email.to_string(),
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
        };
        let someone_elses_unpublished_post = Post {
            author: "someone@el.se".to_string(),
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
        };
        let published_post = Post {
            author: "someone@el.se".to_string(),
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: true,
        };

        // act
        let result_my_unpublished_post = consult_post(&subject, &my_unpublished_post);
        let result_someone_elses_unpublished_post = consult_post(&subject, &someone_elses_unpublished_post);
        let result_published = consult_post(&subject, &published_post);

        // assert
        assert!(matches!(result_my_unpublished_post, ConsultPostResult::DoConsultPost));
        assert!(matches!(result_someone_elses_unpublished_post, ConsultPostResult::DoConsultPost));
        assert!(matches!(result_published, ConsultPostResult::DoConsultPost));
    }

    
}
