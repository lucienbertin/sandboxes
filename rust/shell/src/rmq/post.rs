use serde::{Serialize, Deserialize};
use crate::error::Error;
use super::RmQPublisher;

#[derive(Serialize, Deserialize)]
pub struct User {
    pub first_name: String,
    pub last_name: String,
    pub email: String,
}
#[derive(Serialize, Deserialize)]
pub struct Post {
    pub id: i32,
    pub title: String,
    pub body: String,
    pub author: User,
}

impl From<&domain::models::User> for User {
    fn from(value: &domain::models::User) -> Self {
        Self {
            email: value.email.clone(),
            first_name: value.first_name.clone(),
            last_name: value.last_name.clone(),
        }
    }
}

impl From<&domain::models::Post> for Post {
    fn from(value: &domain::models::Post) -> Self {
        let author = User::from(&value.author);
        Self {
            id: value.id,
            title: value.title.clone(),
            body: value.body.clone(),
            author: author,
        }
    }
}

pub fn notify_post_published(publisher: &RmQPublisher, post: &domain::models::Post) -> Result<(), Error> {
    let p = Post::from(post);
    let message = serde_json::to_string(&p)?;
    publisher.publish("evt.post.pulished".to_string(), message)?;

    Ok(())
}

pub fn notify_post_deleted(publisher: &RmQPublisher, post: &domain::models::Post) -> Result<(), Error> {
    let p = Post::from(post);
    let message = serde_json::to_string(&p)?;
    publisher.publish("evt.post.deleted".to_string(), message)?;

    Ok(())
}

pub fn notify_post_updated(publisher: &RmQPublisher, post: &domain::models::Post) -> Result<(), Error> {
    let p = Post::from(post);
    let message = serde_json::to_string(&p)?;
    publisher.publish("evt.post.updated".to_string(), message)?;

    Ok(())
}