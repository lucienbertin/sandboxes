use crate::models::{Agent, Post, PostEdition, Role, User};

#[derive(PartialEq, Debug)]
pub enum EditPostResult {
    DoUpdate(i32, PostEdition),
    DoUpdateAndNotify(i32, PostEdition, Post),
    NothingToUpdate,
    CantEditAnotherOnesPost,
    CantEditPublishedPost,
    CantEditAsReader,
    CantEditAsWorker,
}

pub fn edit_post(agent: &Agent, post: &Post, request: PostEdition) -> EditPostResult {
    use EditPostResult::*;
    use Role::*;

    let edits = PostEdition {
        title: request.title.filter(|t| *t != post.title),
        body: request.body.filter(|b| *b != post.body),
    };

    match (agent, post, edits) {
        // check if you have the right to edit
        (Agent::Worker, _, _) => CantEditAsWorker,
        (Agent::User(User { role: Reader, .. }), _, _) => CantEditAsReader,
        (
            Agent::User(User { role: Writer, .. }),
            Post {
                published: true, ..
            },
            _,
        ) => CantEditPublishedPost,
        (
            Agent::User(User {
                role: Writer,
                id: writer_id,
                ..
            }),
            Post { author, .. },
            _,
        ) if author.id != *writer_id => CantEditAnotherOnesPost,

        // check if there is something to do
        (
            _,
            _,
            PostEdition {
                title: None,
                body: None,
            },
        ) => NothingToUpdate,

        (Agent::User(User { role: Writer, .. }), p, edits) => DoUpdate(p.id, edits),
        (_, p, edits) if !p.published => DoUpdate(p.id, edits),

        (_, p, edits) => {
            let e = edits.clone();
            let after_update = Post {
                id: p.clone().id,
                title: e.title.unwrap_or(p.clone().title),
                body: e.body.unwrap_or(p.clone().body),
                published: p.clone().published,
                author: p.clone().author,
            };

            DoUpdateAndNotify(p.id, edits, after_update)
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn as_worker() {
        // arrange
        let subject = User {
            id: 1,
            first_name: "test".to_string(),
            last_name: "test".to_string(),
            email: "test@te.st".to_string(),
            role: Role::Reader,
        };
        let agent = Agent::Worker;
        let post = Post {
            author: subject.clone(),
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
        };
        let request = PostEdition {
            title: Some("test".to_string()),
            body: Some("test".to_string()),
        };

        // act
        let result = edit_post(&agent, &post, request);

        // assert
        assert_eq!(result, EditPostResult::CantEditAsWorker);
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
        let post = Post {
            author: subject.clone(),
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
        };
        let request = PostEdition {
            title: Some("test".to_string()),
            body: Some("test".to_string()),
        };

        // act
        let result = edit_post(&agent, &post, request);

        // assert
        assert_eq!(result, EditPostResult::CantEditAsReader);
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
            author: someoneelse.clone(),
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
        };
        let request = PostEdition {
            title: Some("test".to_string()),
            body: Some("test".to_string()),
        };

        // act
        let result = edit_post(&agent, &post, request);

        // assert
        assert_eq!(result, EditPostResult::CantEditAnotherOnesPost);
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
            published: true,
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            author: subject.clone(),
        };
        let request = PostEdition {
            title: Some("test".to_string()),
            body: Some("test".to_string()),
        };

        // act
        let result = edit_post(&agent, &post, request);

        // assert
        assert_eq!(result, EditPostResult::CantEditPublishedPost);
    }

    #[test]
    fn nothing_to_edit() {
        // arrange
        let subject = User {
            id: 1,
            first_name: "test".to_string(),
            last_name: "test".to_string(),
            email: "test@te.st".to_string(),
            role: Role::Writer,
        };
        let agent = Agent::User(subject.clone());

        let title = "title";
        let body = "body";
        let post = Post {
            id: 1,
            title: title.to_string(),
            body: body.to_string(),
            published: false,
            author: subject.clone(),
        };
        let no_edits = PostEdition {
            title: None,
            body: None,
        };
        let same_title = PostEdition {
            title: Some(title.to_string()),
            body: None,
        };
        let same_body = PostEdition {
            title: None,
            body: Some(body.to_string()),
        };
        let same_title_and_body = PostEdition {
            title: Some(title.to_string()),
            body: Some(body.to_string()),
        };

        // act
        let result_no_edits = edit_post(&agent, &post, no_edits);
        let result_same_title = edit_post(&agent, &post, same_title);
        let result_same_body = edit_post(&agent, &post, same_body);
        let result_same_title_and_body = edit_post(&agent, &post, same_title_and_body);

        // assert
        assert_eq!(result_no_edits, EditPostResult::NothingToUpdate);
        assert_eq!(result_same_title, EditPostResult::NothingToUpdate);
        assert_eq!(result_same_body, EditPostResult::NothingToUpdate);
        assert_eq!(result_same_title_and_body, EditPostResult::NothingToUpdate);
    }

    #[test]
    fn happy_path_as_writer() {
        // arrange
        let subject = User {
            id: 1,
            first_name: "test".to_string(),
            last_name: "test".to_string(),
            email: "test@te.st".to_string(),
            role: Role::Writer,
        };
        let agent = Agent::User(subject.clone());

        let title = "title";
        let different_title = "different title";
        let different_body = "different body";
        let body = "body";
        let id = 1i32;
        let post = Post {
            id: id,
            title: title.to_string(),
            body: body.to_string(),
            published: false,
            author: subject.clone(),
        };
        let diff_title_no_body = PostEdition {
            title: Some(different_title.to_string()),
            body: None,
        };
        let diff_title_same_body = PostEdition {
            title: Some(different_title.to_string()),
            body: Some(body.to_string()),
        };
        let no_title_diff_body = PostEdition {
            title: None,
            body: Some(different_body.to_string()),
        };
        let same_title_diff_body = PostEdition {
            title: Some(title.to_string()),
            body: Some(different_body.to_string()),
        };
        let diff_title_diff_body = PostEdition {
            title: Some(different_title.to_string()),
            body: Some(different_body.to_string()),
        };

        // act
        let result_diff_title_no_body = edit_post(&agent, &post, diff_title_no_body);
        let result_diff_title_same_body = edit_post(&agent, &post, diff_title_same_body);
        let result_no_title_diff_body = edit_post(&agent, &post, no_title_diff_body);
        let result_same_title_diff_body = edit_post(&agent, &post, same_title_diff_body);
        let result_diff_title_diff_body = edit_post(&agent, &post, diff_title_diff_body);

        // assert
        assert!(matches!(
            result_diff_title_no_body,
            EditPostResult::DoUpdate(_, _)
        ));
        if let EditPostResult::DoUpdate(id_to_edit, edition) = result_diff_title_no_body {
            assert_eq!(id_to_edit, id);
            assert_eq!(edition.title, Some(different_title.to_string()));
            assert_eq!(edition.body, None);
        }
        assert!(matches!(
            result_diff_title_same_body,
            EditPostResult::DoUpdate(_, _)
        ));
        if let EditPostResult::DoUpdate(id_to_edit, edition) = result_diff_title_same_body {
            assert_eq!(id_to_edit, id);
            assert_eq!(edition.title, Some(different_title.to_string()));
            assert_eq!(edition.body, None);
        }
        assert!(matches!(
            result_no_title_diff_body,
            EditPostResult::DoUpdate(_, _)
        ));
        if let EditPostResult::DoUpdate(id_to_edit, edition) = result_no_title_diff_body {
            assert_eq!(id_to_edit, id);
            assert_eq!(edition.title, None);
            assert_eq!(edition.body, Some(different_body.to_string()));
        }
        assert!(matches!(
            result_same_title_diff_body,
            EditPostResult::DoUpdate(_, _)
        ));
        if let EditPostResult::DoUpdate(id_to_edit, edition) = result_same_title_diff_body {
            assert_eq!(id_to_edit, id);
            assert_eq!(edition.title, None);
            assert_eq!(edition.body, Some(different_body.to_string()));
        }
        assert!(matches!(
            result_diff_title_diff_body,
            EditPostResult::DoUpdate(_, _)
        ));
        if let EditPostResult::DoUpdate(id_to_edit, edition) = result_diff_title_diff_body {
            assert_eq!(id_to_edit, id);
            assert_eq!(edition.title, Some(different_title.to_string()));
            assert_eq!(edition.body, Some(different_body.to_string()));
        }
    }
    #[test]
    fn happy_path_as_admin_published() {
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

        let title = "title";
        let different_title = "different title";
        let different_body = "different body";
        let body = "body";
        let id = 1i32;
        let someone_else_published_post = Post {
            id: id,
            title: title.to_string(),
            body: body.to_string(),
            published: true,
            author: someoneelse.clone(),
        };
        let diff_title_no_body = PostEdition {
            title: Some(different_title.to_string()),
            body: None,
        };
        let diff_title_same_body = PostEdition {
            title: Some(different_title.to_string()),
            body: Some(body.to_string()),
        };
        let no_title_diff_body = PostEdition {
            title: None,
            body: Some(different_body.to_string()),
        };
        let same_title_diff_body = PostEdition {
            title: Some(title.to_string()),
            body: Some(different_body.to_string()),
        };
        let diff_title_diff_body = PostEdition {
            title: Some(different_title.to_string()),
            body: Some(different_body.to_string()),
        };

        // act
        let result_diff_title_no_body =
            edit_post(&agent, &someone_else_published_post, diff_title_no_body);
        let result_diff_title_same_body =
            edit_post(&agent, &someone_else_published_post, diff_title_same_body);
        let result_no_title_diff_body =
            edit_post(&agent, &someone_else_published_post, no_title_diff_body);
        let result_same_title_diff_body =
            edit_post(&agent, &someone_else_published_post, same_title_diff_body);
        let result_diff_title_diff_body =
            edit_post(&agent, &someone_else_published_post, diff_title_diff_body);

        // assert
        assert!(matches!(
            result_diff_title_no_body,
            EditPostResult::DoUpdateAndNotify(_, _, _)
        ));
        if let EditPostResult::DoUpdateAndNotify(id_to_edit, edition, _) = result_diff_title_no_body
        {
            assert_eq!(id_to_edit, id);
            assert_eq!(edition.title, Some(different_title.to_string()));
            assert_eq!(edition.body, None);
        }
        assert!(matches!(
            result_diff_title_same_body,
            EditPostResult::DoUpdateAndNotify(_, _, _)
        ));
        if let EditPostResult::DoUpdateAndNotify(id_to_edit, edition, _) =
            result_diff_title_same_body
        {
            assert_eq!(id_to_edit, id);
            assert_eq!(edition.title, Some(different_title.to_string()));
            assert_eq!(edition.body, None);
        }
        assert!(matches!(
            result_no_title_diff_body,
            EditPostResult::DoUpdateAndNotify(_, _, _)
        ));
        if let EditPostResult::DoUpdateAndNotify(id_to_edit, edition, _) = result_no_title_diff_body
        {
            assert_eq!(id_to_edit, id);
            assert_eq!(edition.title, None);
            assert_eq!(edition.body, Some(different_body.to_string()));
        }
        assert!(matches!(
            result_same_title_diff_body,
            EditPostResult::DoUpdateAndNotify(_, _, _)
        ));
        if let EditPostResult::DoUpdateAndNotify(id_to_edit, edition, _) =
            result_same_title_diff_body
        {
            assert_eq!(id_to_edit, id);
            assert_eq!(edition.title, None);
            assert_eq!(edition.body, Some(different_body.to_string()));
        }
        assert!(matches!(
            result_diff_title_diff_body,
            EditPostResult::DoUpdateAndNotify(_, _, _)
        ));
        if let EditPostResult::DoUpdateAndNotify(id_to_edit, edition, _) =
            result_diff_title_diff_body
        {
            assert_eq!(id_to_edit, id);
            assert_eq!(edition.title, Some(different_title.to_string()));
            assert_eq!(edition.body, Some(different_body.to_string()));
        }
    }
    #[test]
    fn happy_path_as_admin_unpublished() {
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

        let title = "title";
        let different_title = "different title";
        let different_body = "different body";
        let body = "body";
        let id = 1i32;
        let someone_else_published_post = Post {
            id: id,
            title: title.to_string(),
            body: body.to_string(),
            published: false,
            author: someoneelse.clone(),
        };
        let diff_title_no_body = PostEdition {
            title: Some(different_title.to_string()),
            body: None,
        };
        let diff_title_same_body = PostEdition {
            title: Some(different_title.to_string()),
            body: Some(body.to_string()),
        };
        let no_title_diff_body = PostEdition {
            title: None,
            body: Some(different_body.to_string()),
        };
        let same_title_diff_body = PostEdition {
            title: Some(title.to_string()),
            body: Some(different_body.to_string()),
        };
        let diff_title_diff_body = PostEdition {
            title: Some(different_title.to_string()),
            body: Some(different_body.to_string()),
        };

        // act
        let result_diff_title_no_body =
            edit_post(&agent, &someone_else_published_post, diff_title_no_body);
        let result_diff_title_same_body =
            edit_post(&agent, &someone_else_published_post, diff_title_same_body);
        let result_no_title_diff_body =
            edit_post(&agent, &someone_else_published_post, no_title_diff_body);
        let result_same_title_diff_body =
            edit_post(&agent, &someone_else_published_post, same_title_diff_body);
        let result_diff_title_diff_body =
            edit_post(&agent, &someone_else_published_post, diff_title_diff_body);

        // assert
        assert!(matches!(
            result_diff_title_no_body,
            EditPostResult::DoUpdate(_, _)
        ));
        if let EditPostResult::DoUpdate(id_to_edit, edition) = result_diff_title_no_body {
            assert_eq!(id_to_edit, id);
            assert_eq!(edition.title, Some(different_title.to_string()));
            assert_eq!(edition.body, None);
        }
        assert!(matches!(
            result_diff_title_same_body,
            EditPostResult::DoUpdate(_, _)
        ));
        if let EditPostResult::DoUpdate(id_to_edit, edition) = result_diff_title_same_body {
            assert_eq!(id_to_edit, id);
            assert_eq!(edition.title, Some(different_title.to_string()));
            assert_eq!(edition.body, None);
        }
        assert!(matches!(
            result_no_title_diff_body,
            EditPostResult::DoUpdate(_, _)
        ));
        if let EditPostResult::DoUpdate(id_to_edit, edition) = result_no_title_diff_body {
            assert_eq!(id_to_edit, id);
            assert_eq!(edition.title, None);
            assert_eq!(edition.body, Some(different_body.to_string()));
        }
        assert!(matches!(
            result_same_title_diff_body,
            EditPostResult::DoUpdate(_, _)
        ));
        if let EditPostResult::DoUpdate(id_to_edit, edition) = result_same_title_diff_body {
            assert_eq!(id_to_edit, id);
            assert_eq!(edition.title, None);
            assert_eq!(edition.body, Some(different_body.to_string()));
        }
        assert!(matches!(
            result_diff_title_diff_body,
            EditPostResult::DoUpdate(_, _)
        ));
        if let EditPostResult::DoUpdate(id_to_edit, edition) = result_diff_title_diff_body {
            assert_eq!(id_to_edit, id);
            assert_eq!(edition.title, Some(different_title.to_string()));
            assert_eq!(edition.body, Some(different_body.to_string()));
        }
    }
}
