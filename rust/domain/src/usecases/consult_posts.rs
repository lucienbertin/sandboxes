use crate::models::{Agent, Role, User};

#[derive(PartialEq, Debug)]
pub enum ConsultPostsResult {
    ConsultPublishedPosts,
    ConsultPublishedPostsAndAuthoredBy(User),
    ConsultAllPosts,
}
pub fn consult_posts(agent: &Agent) -> ConsultPostsResult {
    use ConsultPostsResult::*;
    match agent {
        Agent::Worker => ConsultAllPosts,
        Agent::User(User { role: Role::Admin, .. }) => ConsultAllPosts,
        Agent::User(writer) if writer.role == Role::Writer => ConsultPublishedPostsAndAuthoredBy(writer.clone()),
        _ => ConsultPostsResult::ConsultPublishedPosts,
    }
}
#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn as_reader() {
        // arrange
        let reader = Agent::User(User {
            id: 1,
            first_name: "test".to_string(),
            last_name: "test".to_string(),
            email: "test@te.st".to_string(),
            role: Role::Reader,
        });
        // act
        let result = consult_posts(&reader);
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
        let agent = Agent::User(writer.clone());
        // act
        let result = consult_posts(&agent);
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
        let agent = Agent::User(User {
            id: 1,
            first_name: "test".to_string(),
            last_name: "test".to_string(),
            email: "test@te.st".to_string(),
            role: Role::Admin,
        });
        // act
        let result = consult_posts(&agent);
        // assert
        assert!(matches!(result, ConsultPostsResult::ConsultAllPosts));
    }

    #[test]
    fn as_worker() {
        // arrange
        let agent = Agent::Worker;
        // act
        let result = consult_posts(&agent);
        // assert
        assert!(matches!(result, ConsultPostsResult::ConsultAllPosts));
    }
}
