use crate::models::{Role, User};

#[derive(PartialEq, Debug)]
pub enum ConsultPostsResult {
    ConsultPublishedPosts,
    ConsultPublishedPostsAndAuthoredBy(User),
    ConsultAllPosts,
}
pub fn consult_posts(subject: &User) -> ConsultPostsResult {
    match subject.role {
        Role::Reader => ConsultPostsResult::ConsultPublishedPosts,
        Role::Writer => ConsultPostsResult::ConsultPublishedPostsAndAuthoredBy(subject.clone()),
        Role::Admin => ConsultPostsResult::ConsultAllPosts,
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
        // act
        let result = consult_posts(&subject);
        // assert
        assert!(matches!(result, ConsultPostsResult::ConsultPublishedPosts));
    }

    #[test]
    fn as_writer() {
        // arrange
        let writer = User {
            id: 1,
            first_name: "test".to_string(),
            last_name: "test".to_string(),
            email: "test@te.st".to_string(),
            role: Role::Writer,
        };
        let subject = writer.clone();
        // act
        let result = consult_posts(&subject);
        // assert
        assert!(matches!(
            result,
            ConsultPostsResult::ConsultPublishedPostsAndAuthoredBy(_)
        ));
        if let ConsultPostsResult::ConsultPublishedPostsAndAuthoredBy(author) = result {
            assert_eq!(author, writer);
        }
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
        // act
        let result = consult_posts(&subject);
        // assert
        assert!(matches!(result, ConsultPostsResult::ConsultAllPosts));
    }
}
