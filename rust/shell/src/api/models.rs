use rocket::serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub struct Post {
    pub id: i32,
    pub title: String,
    pub body: String,
    pub published: bool,
    pub author: String,
}

impl From<domain::models::Post> for Post {
    fn from(value: domain::models::Post) -> Self {
        Self {
            id: value.id,
            title: value.title,
            body: value.body,
            published: value.published,
            author: value.author,
        }
    }
}

#[derive(FromForm)]
pub struct NewPost {
    title: String,
    body: String,
}
impl From<NewPost> for domain::models::NewPostRequest {
    fn from(value: NewPost) -> Self {
        Self {
            title: value.title,
            body: value.body,
        }
    }
}

#[derive(FromForm)]
pub struct PatchPost {
    title: Option<String>,
    body: Option<String>,
}
impl From<PatchPost> for domain::models::PostEdition {
    fn from(value: PatchPost) -> Self {
        Self {
            title: value.title,
            body: value.body,
        }
    }
}
