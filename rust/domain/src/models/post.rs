use super::User;

#[derive(PartialEq, Debug, Clone)]
pub struct Post {
    pub id: i32,
    pub title: String,
    pub body: String,
    pub published: bool,
    pub author: User,
    // pub geom: Point,
}

#[derive(PartialEq, Debug)]
pub struct NewPostRequest {
    pub title: String,
    pub body: String,
}

#[derive(PartialEq, Debug)]
pub struct NewPost {
    pub title: String,
    pub body: String,
    pub author: User,
}

#[derive(PartialEq, Debug)]
pub struct PostEdition {
    pub title: Option<String>,
    pub body: Option<String>,
}
