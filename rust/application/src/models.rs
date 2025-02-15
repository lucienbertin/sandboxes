pub struct Post {
    pub id: i32,
    pub title: String,
    pub body: String,
    pub published: bool,
    pub author: String,
    // pub geom: Point,
}

pub struct NewPostRequest {
    pub title: String,
    pub body: String,
}
pub struct NewPost {
    pub title: String,
    pub body: String,
    pub author: String,
}

pub struct PostEdition {
    pub title: Option<String>,
    pub body: Option<String>,
}