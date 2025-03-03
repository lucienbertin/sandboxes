use diesel::prelude::*;
use postgis_diesel::types::*;

use crate::db::schema::posts;

#[derive(Queryable, Selectable, Clone)]
#[diesel(table_name = posts)]
#[diesel(check_for_backend(diesel::pg::Pg))]
pub struct Post {
    pub id: i32,
    pub title: String,
    pub body: String,
    pub published: bool,
    pub geom: Option<Point>,
    pub author: String,
}

#[derive(Insertable)]
#[diesel(table_name = posts)]
pub struct NewPost {
    pub title: String,
    pub body: String,
    pub author: String,
}

impl From<domain::models::Post> for Post {
    fn from(value: domain::models::Post) -> Self {
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
impl From<Post> for domain::models::Post {
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
impl From<domain::models::NewPost> for NewPost {
    fn from(value: domain::models::NewPost) -> Self {
        Self {
            title: value.title,
            body: value.body,
            author: value.author,
        }
    }
}
