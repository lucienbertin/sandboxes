pub struct Post {
    pub id: i32,
    pub title: String,
    pub body: String,
    pub published: bool,
    pub author: String,
    // pub geom: Point,
}

pub struct NewPost {
    pub title: String,
    pub body: String,
}