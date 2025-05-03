use super::RmQPublisher;
use crate::error::Error;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub struct Email {
    pub to: String,
    pub title: String,
    pub body: String,
}

pub fn trigger_mail_post_published(
    publisher: &RmQPublisher,
    post: &domain::models::Post,
    author: &domain::models::User,
) -> Result<(), Error> {
    let email = Email {
        to: author.email.clone(),
        title: format!("Your post '{}' has been published", &post.title),
        body: format!(
            "Your post '{}' has been published by an admin and we just wanted to let you know",
            &post.title
        ),
    };
    let message = serde_json::to_string(&email)?;
    publisher.publish("job.sendmail".to_string(), message)?;

    Ok(())
}
