use diesel::prelude::*;
use postgis_diesel::types::*;

#[derive(Queryable, Selectable)]
#[derive(Clone)]
#[diesel(table_name = crate::schema::posts)]
#[diesel(check_for_backend(diesel::pg::Pg))]
pub struct Post {
    pub id: i32,
    pub title: String,
    pub body: String,
    pub published: bool,
    pub geom: Option<Point>,
    pub author: String,
}

use crate::schema::posts;
#[derive(Insertable)]
#[diesel(table_name = posts)]
pub struct NewPost {
    pub title: String,
    pub body: String,
}

impl From<application::models::Post> for Post {
    fn from(value: application::models::Post) -> Self {
        Self {
            id: value.id,
            title: value.title,
            body: value.body,
            published: value.published,
            geom: None, // not binded for now
            author: value.author,
        }
    }
}
impl From<Post> for application::models::Post {
    fn from(value: Post) -> Self {
        Self {
            id: value.id,
            title: value.title,
            body: value.body,
            published: value.published,
            author: value.author,
        }
    }
}
impl From<application::models::NewPost> for NewPost {
    fn from(value: application::models::NewPost) -> Self {
        Self {
            title: value.title,
            body: value.body,
        }
    }
}