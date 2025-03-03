use std::io::Write;

use diesel::*;
use diesel::deserialize::{FromSql, FromSqlRow};
use diesel::pg::{Pg, PgValue};
use diesel::serialize::{IsNull, Output, ToSql};
use diesel::expression::AsExpression;

use crate::db::schema::users;

#[derive(Debug, PartialEq, FromSqlRow, AsExpression, Eq, Clone)]
#[diesel(sql_type = crate::db::schema::sql_types::UserRole)]
pub enum Role {
    Admin,
    Writer,
    Reader,
}

impl ToSql<crate::db::schema::sql_types::UserRole, Pg> for Role {
    fn to_sql<'b>(&'b self, out: &mut Output<'b, '_, Pg>) -> serialize::Result {
        match *self {
            Role::Admin => out.write_all(b"admin")?,
            Role::Writer => out.write_all(b"wirter")?,
            Role::Reader => out.write_all(b"reader")?,
        }
        Ok(IsNull::No)
    }
}

impl FromSql<crate::db::schema::sql_types::UserRole, Pg> for Role {
    fn from_sql(bytes: PgValue<'_>) -> deserialize::Result<Self> {
        match bytes.as_bytes() {
            b"admin" => Ok(Role::Admin),
            b"wirter" => Ok(Role::Writer),
            b"reader" => Ok(Role::Reader),
            _ => Err("Unrecognized enum variant".into()),
        }
    }
}

#[derive(Queryable, Selectable, Clone)]
#[diesel(table_name = users)]
#[diesel(check_for_backend(diesel::pg::Pg))]
pub struct User {
    pub id: i32,
    pub first_name: String,
    pub last_name: String,
    pub email: String,
    pub role: Role,
}
impl From<domain::models::User> for User {
    fn from(value: domain::models::User) -> Self {
        Self {
            id: value.id,
            first_name: value.first_name,
            last_name: value.last_name,
            email: value.email,
            role: match value.role {
                domain::models::Role::Admin => Role::Admin,
                domain::models::Role::Writer => Role::Writer,
                domain::models::Role::Reader => Role::Reader,
            },
        }
    }
}
impl From<User> for domain::models::User {
    fn from(value: User) -> Self {
        Self {
            id: value.id,
            first_name: value.first_name,
            last_name: value.last_name,
            email: value.email,
            role: match value.role {
                Role::Admin => domain::models::Role::Admin,
                Role::Writer => domain::models::Role::Writer,
                Role::Reader => domain::models::Role::Reader,
            },
        }
    }
}

use crate::error::Error;
pub fn find_user(
    connection: &mut PgConnection,
    user_id: i32,
) -> Result<Option<domain::models::User>, Error> {
    use crate::db::schema::users::dsl::*;

    let result: Option<User> = users
        .find(user_id)
        .select(User::as_select())
        .first(connection)
        .optional()?;
    let result = result.map(|u| u.into());

    Ok(result)
}

#[cfg(test)]
mod test {
    use diesel::{Connection, PgConnection};

    use crate::error::Error;
    fn establish_connection() -> Result<PgConnection, Error> {
        use dotenvy::dotenv;
        use std::env;

        dotenv()?;
        let database_url = env::var("DATABASE_URL")?;
        let connection = PgConnection::establish(&database_url)?;

        Ok(connection)
    }

    #[test]
    fn find_user() {
        let mut conn = establish_connection().expect("cant connect to db");
        let res_0 = super::find_user(&mut conn, 0);
        let res_1 = super::find_user(&mut conn, 1);
        let res_2 = super::find_user(&mut conn, 2);

        println!("user 0: {:?}", res_0);
        println!("user 1: {:?}", res_1);
        println!("user 2: {:?}", res_2);
    }
}
