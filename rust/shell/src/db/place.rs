use crate::error::Error;
use diesel::prelude::*;
use postgis_diesel::types::Point;

// Bindings
use crate::db::schema::places;
// use crate::db::schema::places::dsl::*;
#[derive(Queryable, Insertable, Identifiable, Selectable)]
#[diesel(table_name = places)]
#[diesel(check_for_backend(diesel::pg::Pg))]
struct Place {
    pub id: i32,
    pub name: String,
    pub geometry: Point,
}

// from-into with domain layer
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
impl Into<domain::models::Place> for Place {
    fn into(self) -> domain::models::Place {
        let point: geojson::PointType = vec![self.geometry.x, self.geometry.y];

        domain::models::Place {
            id: self.id,
            name: self.name,
            point: point,
        }
    }
}

pub fn select_places(connection: &mut PgConnection) -> Result<Vec<domain::models::Place>, Error> {
    let results: Vec<Place> = places::table.select(Place::as_select()).load(connection)?;

    let results = results.into_iter().map(|p| p.into()).collect();

    Ok(results)
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
