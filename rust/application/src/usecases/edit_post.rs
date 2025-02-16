use crate::models::{Post, PostEdition};

#[derive(PartialEq, Debug)]
pub enum EditPostResult {
    DoUpdate(i32, PostEdition),
    NothingToUpdate,
    CantEditAnotherOnesPost,
    CantEditPublishedPost,
}

pub fn edit_post(subject: &String, post: &Post, request: PostEdition) -> EditPostResult {
    let check_subject = match subject.as_str() {
        s if s != &post.author => Some(EditPostResult::CantEditAnotherOnesPost),
        _ => None
    };

    let check_post = match post.published {
        true => Some(EditPostResult::CantEditPublishedPost),
        false => None,
    };

    let edits = PostEdition{ 
        title: request.title.filter(|t| *t != post.title),
        body: request.body.filter(|b| *b != post.body),
    };
    let check_edits = match edits {
        PostEdition{ title: None, body: None } => Some(EditPostResult::NothingToUpdate),
        _ => None
    };

    let result = check_subject
        .or(check_post)
        .or(check_edits)
        .or(Some(EditPostResult::DoUpdate(post.id, edits)))
        .unwrap();

    result
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn wrong_author() {
        // arrange
        let subject = "test@te.st".to_string();
        let post = Post{ author: "someone@el.se".to_string(), id: 1, title: "test".to_string(), body: "test".to_string(), published: false, };
        let request = PostEdition{ title: Some("test".to_string()), body: Some("test".to_string()) };

        // act
        let result = edit_post(&subject, &post, request);

        // assert
        assert_eq!(result, EditPostResult::CantEditAnotherOnesPost);
    }

    #[test]
    fn already_published() {
        // arrange
        let subject = "test@te.st".to_string();
        let post = Post{ published: true, id: 1, title: "test".to_string(), body: "test".to_string(), author: subject.clone() };
        let request = PostEdition{ title: Some("test".to_string()), body: Some("test".to_string()) };

        // act
        let result = edit_post(&subject, &post, request);

        // assert
        assert_eq!(result, EditPostResult::CantEditPublishedPost);
    }

    #[test]
    fn nothing_to_edit() {
        // arrange
        let subject = "test@te.st".to_string();
        let title = "title";
        let body = "body";
        let post = Post{ id: 1, title: title.to_string(), body: body.to_string(), published: false, author: subject.clone() };
        let no_edits = PostEdition{ title: None, body: None };
        let same_title = PostEdition{ title: Some(title.to_string()), body: None };
        let same_body = PostEdition{ title: None , body: Some(body.to_string()) };
        let same_title_and_body = PostEdition{ title: Some(title.to_string()) , body: Some(body.to_string()) };

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
    fn happy_path() {
        // arrange
        let subject = "test@te.st".to_string();
        let title = "title";
        let different_title = "different title";
        let different_body = "different body";
        let body = "body";
        let id = 1i32;
        let post = Post{ id: id, title: title.to_string(), body: body.to_string(), published: false, author: subject.clone() };
        let diff_title_no_body = PostEdition{ title: Some(different_title.to_string()), body: None };
        let diff_title_same_body = PostEdition{ title: Some(different_title.to_string()), body: Some(body.to_string()) };
        let no_title_diff_body = PostEdition{ title: None, body: Some(different_body.to_string()) };
        let same_title_diff_body = PostEdition{ title: Some(title.to_string()), body: Some(different_body.to_string()) };
        let diff_title_diff_body = PostEdition{ title: Some(different_title.to_string()), body: Some(different_body.to_string()) };

        // act
        let result_diff_title_no_body = edit_post(&subject, &post, diff_title_no_body);
        let result_diff_title_same_body = edit_post(&subject, &post, diff_title_same_body);
        let result_no_title_diff_body = edit_post(&subject, &post, no_title_diff_body);
        let result_same_title_diff_body = edit_post(&subject, &post, same_title_diff_body);
        let result_diff_title_diff_body = edit_post(&subject, &post, diff_title_diff_body);

        // assert
        assert!(matches!(result_diff_title_no_body, EditPostResult::DoUpdate(_, _)));
        if let EditPostResult::DoUpdate(id_to_edit, edition) = result_diff_title_no_body {
            assert_eq!(id_to_edit, id);
            assert_eq!(edition.title, Some(different_title.to_string()));
            assert_eq!(edition.body, None);
        }
        assert!(matches!(result_diff_title_same_body, EditPostResult::DoUpdate(_, _)));
        if let EditPostResult::DoUpdate(id_to_edit, edition) = result_diff_title_same_body {
            assert_eq!(id_to_edit, id);
            assert_eq!(edition.title, Some(different_title.to_string()));
            assert_eq!(edition.body, None);
        }
        assert!(matches!(result_no_title_diff_body, EditPostResult::DoUpdate(_, _)));
        if let EditPostResult::DoUpdate(id_to_edit, edition) = result_no_title_diff_body {
            assert_eq!(id_to_edit, id);
            assert_eq!(edition.title, None);
            assert_eq!(edition.body, Some(different_body.to_string()));
        }
        assert!(matches!(result_same_title_diff_body, EditPostResult::DoUpdate(_, _)));
        if let EditPostResult::DoUpdate(id_to_edit, edition) = result_same_title_diff_body {
            assert_eq!(id_to_edit, id);
            assert_eq!(edition.title, None);
            assert_eq!(edition.body, Some(different_body.to_string()));
        }
        assert!(matches!(result_diff_title_diff_body, EditPostResult::DoUpdate(_, _)));
        if let EditPostResult::DoUpdate(id_to_edit, edition) = result_diff_title_diff_body {
            assert_eq!(id_to_edit, id);
            assert_eq!(edition.title, Some(different_title.to_string()));
            assert_eq!(edition.body, Some(different_body.to_string()));
        }
    }
}