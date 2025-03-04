use diesel::prelude::*;
// use postgis_diesel::types::*;
use super::user::User;
use crate::error::Error;
// Bindings
use crate::db::schema::posts;
use crate::db::schema::posts::dsl::*;
use crate::db::schema::users;
#[derive(Queryable, Selectable, Clone, Identifiable, Associations)]
#[diesel(belongs_to(User, foreign_key = author_id))]
#[diesel(table_name = posts)]
#[diesel(check_for_backend(diesel::pg::Pg))]
struct Post {
    pub id: i32,
    pub title: String,
    pub body: String,
    pub published: bool,
    // pub geom: Option<Point>,
    pub author_id: i32,
}

#[derive(Insertable)]
#[diesel(table_name = posts)]
struct NewPost {
    pub title: String,
    pub body: String,
    pub author_id: i32,
}

impl From<domain::models::Post> for Post {
    fn from(value: domain::models::Post) -> Self {
        Self {
            id: value.id,
            title: value.title,
            body: value.body,
            published: value.published,
            // geom: None, // not binded for now
            author_id: value.author.id,
        }
    }
}
struct PostAuthorTuple(Post, User);
impl Into<domain::models::Post> for PostAuthorTuple {
    fn into(self) -> domain::models::Post {
        domain::models::Post {
            id: self.0.id,
            title: self.0.title,
            body: self.0.body,
            published: self.0.published,
            author: self.1.into(),
        }
    }
}
impl From<(Post, User)> for PostAuthorTuple {
    fn from(value: (Post, User)) -> Self {
        Self(value.0, value.1)
    }
}
impl From<domain::models::NewPost> for NewPost {
    fn from(value: domain::models::NewPost) -> Self {
        Self {
            title: value.title,
            body: value.body,
            author_id: value.author.id,
        }
    }
}

// queries
pub fn select_posts(connection: &mut PgConnection) -> Result<Vec<domain::models::Post>, Error> {
    // let results: Vec<Post> = posts.select(Post::as_select()).load(connection)?;
    let results: Vec<(Post, User)> = posts::table
        .inner_join(users::table)
        .select((Post::as_select(), User::as_select()))
        .load(connection)?;
    let results: Vec<domain::models::Post> = results
        .into_iter()
        .map(|t| PostAuthorTuple::from(t).into())
        .collect();

    Ok(results)
}
pub fn select_published_posts(
    connection: &mut PgConnection,
) -> Result<Vec<domain::models::Post>, Error> {
    let results: Vec<(Post, User)> = posts::table
        .inner_join(users::table)
        .filter(published.eq(true))
        .select((Post::as_select(), User::as_select()))
        .load(connection)?;
    let results: Vec<domain::models::Post> = results
        .into_iter()
        .map(|t| PostAuthorTuple::from(t).into())
        .collect();

    Ok(results)
}
pub fn select_published_posts_or_authored_by(
    connection: &mut PgConnection,
    user: domain::models::User,
) -> Result<Vec<domain::models::Post>, Error> {
    let results: Vec<(Post, User)> = posts::table
        .inner_join(users::table)
        .filter(published.eq(true).or(author_id.eq(user.id)))
        .select((Post::as_select(), User::as_select()))
        .load(connection)?;
    let results: Vec<domain::models::Post> = results
        .into_iter()
        .map(|t| PostAuthorTuple::from(t).into())
        .collect();

    Ok(results)
}

pub fn find_post(
    connection: &mut PgConnection,
    post_id: i32,
) -> Result<Option<domain::models::Post>, Error> {
    let result: Option<(Post, User)> = posts::table
        .find(post_id)
        .inner_join(users::table)
        .select((Post::as_select(), User::as_select()))
        .first(connection)
        .optional()?;
    let result = result.map(|t| PostAuthorTuple::from(t).into());

    Ok(result)
}

pub fn insert_new_post(
    connection: &mut PgConnection,
    new_post: domain::models::NewPost,
) -> Result<i32, Error> {
    let new_post: NewPost = new_post.into();

    let result = diesel::insert_into(posts::table)
        .values(&new_post)
        .returning(Post::as_returning())
        .get_result(connection)?;

    Ok(result.id)
}

pub fn delete_post(connection: &mut PgConnection, post_id: i32) -> Result<(), Error> {
    diesel::delete(posts.filter(id.eq(post_id))).execute(connection)?;

    Ok(())
}

pub fn publish_post(connection: &mut PgConnection, post_id: i32) -> Result<(), Error> {
    diesel::update(posts.filter(id.eq(post_id)))
        .set(published.eq(true))
        .execute(connection)?;

    Ok(())
}

pub fn update_post(
    connection: &mut PgConnection,
    post_id: i32,
    edits: domain::models::PostEdition,
) -> Result<(), Error> {
    match edits {
        domain::models::PostEdition {
            title: Some(t),
            body: Some(b),
        } => diesel::update(posts.filter(id.eq(post_id)))
            .set((title.eq(t.as_str()), body.eq(b.as_str())))
            .execute(connection),
        domain::models::PostEdition {
            title: None,
            body: Some(b),
        } => diesel::update(posts.filter(id.eq(post_id)))
            .set(body.eq(b.as_str()))
            .execute(connection),
        domain::models::PostEdition {
            title: Some(t),
            body: None,
        } => diesel::update(posts.filter(id.eq(post_id)))
            .set(title.eq(t.as_str()))
            .execute(connection),
        domain::models::PostEdition {
            title: None,
            body: None,
        } => panic!("update_post called with nothing to update"),
    }?;

    Ok(())
}
