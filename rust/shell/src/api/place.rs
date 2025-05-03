use crate::db::{self, find_user, DbConn};
use crate::error::{Error, ResponseError};
use crate::redis::{self, match_etag};
use super::ServerState;
use domain::models::Agent;
use rocket::serde::json::Json;
use rocket::serde::Serialize;
use rocket::State;
use serde::ser::SerializeStruct;
use serde::Serializer;

use super::{EtagJson, IfNoneMatchHeader, JwtIdentifiedSubject};

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
    server_state: &State<ServerState>,
    subject: JwtIdentifiedSubject, // is allowed with no auth
    if_none_match: Option<IfNoneMatchHeader>,
) -> Result<EtagJson<Vec<Place>>, ResponseError> {
    let cache_key = "places".to_string();
    let mut redis_conn = redis::get_conn(&server_state.redis_pool)?;
    let use_cache = if_none_match.map(|er| match_etag(&mut redis_conn, &cache_key, er.etag));
    match use_cache {
        Some(Ok(true)) => Err(Error::NotModified),
        _ => Ok(()),
    }?;

    let mut db_conn = db::get_conn(&server_state.db_pool)?;
    let results = fetch_places(&mut db_conn, subject)?;

    let results = results.into_iter().map(|x| x.into()).collect();
    let etag = match redis::get_etag(&mut redis_conn, &cache_key) {
        Ok(t) => Some(t),
        _ => None, // i dont want to 500 on a redis error when i have the results available
    };

    let result = EtagJson {
        body: Json(results),
        etag: etag,
    };

    Ok(result)
}

#[get("/places", format = "application/geo+json", rank = 2)]
pub async fn get_places_geojson(
    server_state: &State<ServerState>,
    subject: JwtIdentifiedSubject, // is allowed with no auth
    if_none_match: Option<IfNoneMatchHeader>,
) -> Result<EtagJson<PlaceFeatureCollection>, ResponseError> {
    let cache_key = "places-geojson".to_string();
    let mut redis_conn = redis::get_conn(&server_state.redis_pool)?;
    let use_cache = if_none_match.map(|er| match_etag(&mut redis_conn, &cache_key, er.etag));
    match use_cache {
        Some(Ok(true)) => Err(Error::NotModified),
        _ => Ok(()),
    }?;

    let mut db_conn = db::get_conn(&server_state.db_pool)?;
    let results = fetch_places(&mut db_conn, subject)?;

    let features: Vec<PlaceFeature> = results.into_iter().map(|x| x.into()).collect();
    let collection = PlaceFeatureCollection(features);
    let etag = match redis::get_etag(&mut redis_conn, &cache_key) {
        Ok(t) => Some(t),
        _ => None, // i dont want to 500 on a redis error when i have the results available
    };

    let result = EtagJson {
        body: Json(collection),
        etag: etag,
    };

    Ok(result)
}

fn fetch_places(
    db_conn: &mut DbConn,
    subject: JwtIdentifiedSubject, // is allowed with no auth
) -> Result<Vec<domain::models::Place>, Error> {
    let results = db_conn.build_transaction().read_only().run(|conn| {
        let agent = find_user(conn, subject.email)?;
        let agent = agent.ok_or(Error::Unauthorized)?;
        let agent = Agent::User(agent);

        let result = domain::usecases::consult_places(&agent);

        use domain::usecases::ConsultPlacesResult::*;
        match result {
            ConsultAllPlaces => db::select_places(conn),
        }
    })?;

    Ok(results)
}
