use crate::models::{Post, PostEdition, Role, User};

#[derive(PartialEq, Debug)]
pub enum EditPostResult {
    DoUpdate(i32, PostEdition),
    NothingToUpdate,
    CantEditAnotherOnesPost,
    CantEditPublishedPost,
    CantEditAsReader,
}

pub fn edit_post(subject: &User, post: &Post, request: PostEdition) -> EditPostResult {
    let check_subject = match subject.role {
        Role::Writer if subject.email != post.author => {
            Some(EditPostResult::CantEditAnotherOnesPost)
        }
        Role::Writer if post.published => Some(EditPostResult::CantEditPublishedPost),
        Role::Reader => Some(EditPostResult::CantEditAsReader),
        _ => None,
    };

    let edits = PostEdition {
        title: request.title.filter(|t| *t != post.title),
        body: request.body.filter(|b| *b != post.body),
    };
    let check_edits = match edits {
        PostEdition {
            title: None,
            body: None,
        } => Some(EditPostResult::NothingToUpdate),
        _ => None,
    };

    let result = check_subject
        .or(check_edits)
        .or(Some(EditPostResult::DoUpdate(post.id, edits)))
        .unwrap();

    result
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
        let post = Post {
            author: "someone@el.se".to_string(),
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
        let result = edit_post(&subject, &post, request);

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
        let post = Post {
            author: "someone@el.se".to_string(),
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
        let result = edit_post(&subject, &post, request);

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
        let post = Post {
            published: true,
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            author: subject.email.clone(),
        };
        let request = PostEdition {
            title: Some("test".to_string()),
            body: Some("test".to_string()),
        };

        // act
        let result = edit_post(&subject, &post, request);

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

        let title = "title";
        let body = "body";
        let post = Post {
            id: 1,
            title: title.to_string(),
            body: body.to_string(),
            published: false,
            author: subject.email.clone(),
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
        let result_no_edits = edit_post(&subject, &post, no_edits);
        let result_same_title = edit_post(&subject, &post, same_title);
        let result_same_body = edit_post(&subject, &post, same_body);
        let result_same_title_and_body = edit_post(&subject, &post, same_title_and_body);

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
            author: subject.email.clone(),
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
        let result_diff_title_no_body = edit_post(&subject, &post, diff_title_no_body);
        let result_diff_title_same_body = edit_post(&subject, &post, diff_title_same_body);
        let result_no_title_diff_body = edit_post(&subject, &post, no_title_diff_body);
        let result_same_title_diff_body = edit_post(&subject, &post, same_title_diff_body);
        let result_diff_title_diff_body = edit_post(&subject, &post, diff_title_diff_body);

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
    fn happy_path_as_admin() {
        // arrange
        let subject = User {
            id: 1,
            first_name: "test".to_string(),
            last_name: "test".to_string(),
            email: "test@te.st".to_string(),
            role: Role::Admin,
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
            author: "someone@el.se".to_string(),
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
            edit_post(&subject, &someone_else_published_post, diff_title_no_body);
        let result_diff_title_same_body =
            edit_post(&subject, &someone_else_published_post, diff_title_same_body);
        let result_no_title_diff_body =
            edit_post(&subject, &someone_else_published_post, no_title_diff_body);
        let result_same_title_diff_body =
            edit_post(&subject, &someone_else_published_post, same_title_diff_body);
        let result_diff_title_diff_body =
            edit_post(&subject, &someone_else_published_post, diff_title_diff_body);

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
