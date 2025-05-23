use super::{
    check_none_match, get_etag_safe, DbConnWrapper, EtagJson, IfNoneMatchHeader,
    JwtIdentifiedSubject, RedisConnWrapper,
};
use crate::api::resolve_agent;
use crate::db::{self, DbConn};
use crate::error::Error;
use rocket::serde::Serialize;
use serde::ser::SerializeStruct;
use serde::Serializer;

#[derive(Serialize)]
pub struct Place {
    pub id: i32,
    pub name: String,
}

impl From<domain::models::Place> for Place {
    fn from(value: domain::models::Place) -> Self {
        Self {
            id: value.id,
            name: value.name,
        }
    }
}
pub struct PointGeometry(geojson::PointType);
impl Serialize for PointGeometry {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut x = serializer.serialize_struct("PointGeometry", 2)?;
        x.serialize_field("type", "Point")?;
        x.serialize_field("coordinates", &self.0)?;
        x.end()
    }
}
pub struct PlaceFeature(domain::models::Place);
impl Serialize for PlaceFeature {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let place = self.0.clone();
        let mut x = serializer.serialize_struct("PlaceFeature", 4)?;
        x.serialize_field("type", "Feature")?;
        x.serialize_field("id", &place.id)?;
        x.serialize_field("geometry", &PointGeometry(place.point.clone()))?;
        x.serialize_field("properties", &Place::from(place))?;
        x.end()
    }
}

impl From<domain::models::Place> for PlaceFeature {
    fn from(value: domain::models::Place) -> Self {
        Self(value)
    }
}

pub struct PlaceFeatureCollection(Vec<PlaceFeature>);
impl Serialize for PlaceFeatureCollection {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut x = serializer.serialize_struct("PlaceFeatureCollection", 2)?;
        x.serialize_field("type", "FeatureCollection")?;
        x.serialize_field("features", &self.0)?;
        x.end()
    }
}

#[get("/places", format = "json")]
pub async fn get_places(
    mut db_conn: DbConnWrapper,
    mut redis_conn: RedisConnWrapper,
    subject: Option<JwtIdentifiedSubject>,
    if_none_match: Option<IfNoneMatchHeader>,
) -> Result<EtagJson<Vec<Place>>, Error> {
    let cache_key = "places".to_string();
    check_none_match(&mut redis_conn, &cache_key, if_none_match)?;

    let results = fetch_places(&mut db_conn, subject)?;

    let places = results.into_iter().map(|x| x.into()).collect();
    let etag = get_etag_safe(&mut redis_conn, &cache_key);
    let result = EtagJson::new(places, etag);

    Ok(result)
}

#[get("/places", format = "application/geo+json", rank = 2)]
pub async fn get_places_geojson(
    mut db_conn: DbConnWrapper,
    mut redis_conn: RedisConnWrapper,
    subject: Option<JwtIdentifiedSubject>,
    if_none_match: Option<IfNoneMatchHeader>,
) -> Result<EtagJson<PlaceFeatureCollection>, Error> {
    let cache_key = "places-geojson".to_string();
    check_none_match(&mut redis_conn, &cache_key, if_none_match)?;

    let results = fetch_places(&mut db_conn, subject)?;

    let features: Vec<PlaceFeature> = results.into_iter().map(|x| x.into()).collect();
    let collection = PlaceFeatureCollection(features);
    let etag = get_etag_safe(&mut redis_conn, &cache_key);
    let result = EtagJson::new(collection, etag);

    Ok(result)
}

fn fetch_places(
    db_conn: &mut DbConn,
    subject: Option<JwtIdentifiedSubject>,
) -> Result<Vec<domain::models::Place>, Error> {
    let results = db_conn.build_transaction().read_only().run(
        |conn| -> Result<Vec<domain::models::Place>, Error> {
            let agent = resolve_agent(conn, subject)?;
            let result = domain::usecases::consult_places(&agent);
            use domain::usecases::ConsultPlacesResult::*;
            match result {
                ConsultAllPlaces => db::select_places(conn),
            }
        },
    )?;

    Ok(results)
}
