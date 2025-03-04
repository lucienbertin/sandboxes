#[derive(PartialEq, Debug)]
pub struct Post {
    pub id: i32,
    pub title: String,
    pub body: String,
    pub published: bool,
    pub author_id: i32,
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
    pub author_id: i32,
}

#[derive(PartialEq, Debug)]
pub struct PostEdition {
    pub title: Option<String>,
    pub body: Option<String>,
}
