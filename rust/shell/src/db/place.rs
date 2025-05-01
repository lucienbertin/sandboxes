use crate::error::Error;
use diesel::prelude::*;
use postgis_diesel::types::Point;

// Bindings
use crate::db::schema::places;
// use crate::db::schema::places::dsl::*;
#[derive(Queryable, Insertable, Identifiable)]
#[diesel(table_name = places)]
#[diesel(check_for_backend(diesel::pg::Pg))]
struct Place {
    pub id: i32,
    pub name: String,
    pub geometry: Point,
}

impl From<domain::models::Place> for Place {
    fn from(value: domain::models::Place) -> Self {
        let geometry = Point {
            x: value.point[0],
            y: value.point[1],
            srid: None,
        };
        Self {
            id: value.id,
            name: value.name,
            geometry: geometry,
        }
    }
}

pub fn insert_place(
    connection: &mut PgConnection,
    place: domain::models::Place,
) -> Result<(), Error> {
    let p: Place = place.into();

    diesel::insert_into(places::table)
        .values(&p)
        .execute(connection)?;

    Ok(())
}
