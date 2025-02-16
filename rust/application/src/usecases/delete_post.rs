use crate::models::Post;

#[derive(PartialEq, Debug)]
pub enum DeletePostResult {
    DoDelete(i32),
    CantDeletePublishedPost,
    CantDeleteAnotherOnesPost
}
pub fn delete_post(subject: String, post: Post) -> DeletePostResult {
    if subject != post.author {
        return DeletePostResult::CantDeleteAnotherOnesPost; 
    }
    match post.published {
        true => DeletePostResult::CantDeletePublishedPost,
        false => DeletePostResult::DoDelete(post.id),
    }
}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn wrong_author() {
        // arrange
        let subject = "test@te.st".to_string();
        let post = Post{ author: "someone@el.se".to_string(), id: 1, title: "test".to_string(), body: "test".to_string(), published: false };

        // act
        let result: DeletePostResult = delete_post(subject, post);

        // assert
        assert_eq!(result, DeletePostResult::CantDeleteAnotherOnesPost);
    }

    #[test]
    fn already_published() {
        // arrange
        let subject = "test@te.st".to_string();
        let post = Post{ published: true, id: 1, title: "test".to_string(), body: "test".to_string(), author: subject.clone() };

        // act
        let result = delete_post(subject, post);

        // assert
        assert_eq!(result, DeletePostResult::CantDeletePublishedPost);
    }

    #[test]
    fn happy_path() {
        // arrange
        let subject = "test@te.st".to_string();
        let id = 1i32;
        let post = Post{ id: id, title: "test".to_string(), body: "test".to_string(), published: false, author: subject.clone() };

        // act
        let result = delete_post(subject, post);

        // assert
        assert_eq!(result, DeletePostResult::DoDelete(id));
    }
}