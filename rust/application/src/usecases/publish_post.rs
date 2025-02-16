use crate::models::Post;

#[derive(PartialEq, Debug)]
pub enum PublishPostResult {
    DoPublish(i32),
    CantPublishAnotherOnesPost,
    CantPublishAlreadyPublishedPost,
}

pub fn publish_post(subject: &String, post: &Post) -> PublishPostResult {
    match subject.as_str() {
        s if s != &post.author => PublishPostResult::CantPublishAnotherOnesPost,
        _s if post.published => PublishPostResult::CantPublishAlreadyPublishedPost,
        _ => PublishPostResult::DoPublish(post.id),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn wrong_author() {
        // arrange
        let subject = "test@te.st".to_string();
        let post = Post {
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
            author: "someone@el.se".to_string(),
        };

        // act
        let result = publish_post(&subject, &post);

        // assert
        assert_eq!(result, PublishPostResult::CantPublishAnotherOnesPost);
    }

    #[test]
    fn already_published() {
        // arrange
        let subject = "test@te.st".to_string();
        let post = Post {
            id: 1,
            title: "test".to_string(),
            body: "test".to_string(),
            published: true,
            author: subject.clone(),
        };

        // act
        let result = publish_post(&subject, &post);

        // assert
        assert_eq!(result, PublishPostResult::CantPublishAlreadyPublishedPost);
    }

    #[test]
    fn happy_path() {
        // arrange
        let subject = "test@te.st".to_string();
        let id = 1i32;
        let post = Post {
            id: id,
            title: "test".to_string(),
            body: "test".to_string(),
            published: false,
            author: subject.clone(),
        };

        // act
        let result = publish_post(&subject, &post);

        // assert
        assert_eq!(result, PublishPostResult::DoPublish(id));
    }
}
