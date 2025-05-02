use crate::redis;
use domain::{models::Agent, usecases::CreatePlaceResult};
use geojson::{Feature, GeoJson, PointType};
use lapin::message::Delivery;

use crate::error::{Error, GeoJSONSerdeError};

#[derive(Debug)]
pub struct Place {
    id: i32,
    name: String,
    geometry: PointType,
}
impl Into<domain::models::Place> for Place {
    fn into(self) -> domain::models::Place {
        domain::models::Place {
            id: self.id,
            name: self.name,
            point: self.geometry,
        }
    }
}
impl Place {
    pub fn try_from(feature: Feature) -> Result<Self, Error> {
        use GeoJSONSerdeError::*;
        let properties = feature.properties.ok_or(NoProperties)?;
        let geometry = feature.geometry.ok_or(NoGeometry)?;
        let point: PointType = match geometry.value {
            geojson::Value::Point(p) => Ok(p),
            _ => Err(InvalidGeometryType),
        }?;

        use serde_json::value::Value;
        let v_id = properties
            .get("id")
            .ok_or(MissingProperty("id".to_string()))?;
        let v_name = properties
            .get("name")
            .ok_or(MissingProperty("name".to_string()))?;

        let n_id = match v_id {
            Value::Number(n) => Ok(n),
            _ => Err(InvalidPropertyType("id".to_string())),
        }?;
        let id = n_id.as_i64().ok_or(InvalidPropertyType("id".to_string()))?;
        let name = match v_name {
            Value::String(s) => Ok(s.clone()),
            _ => Err(InvalidPropertyType("name".to_string())),
        }?;

        let place = Self {
            id: id as i32,
            name: name,
            geometry: point,
        };

        Ok(place)
    }
}

pub fn handle_create_place(delivery: &Delivery) -> Result<(), Error> {
    print!("place created event | ");

    let data_str = String::from_utf8(delivery.data.clone())?;
    let geojson: GeoJson = data_str.parse::<GeoJson>()?;
    let feature: Feature = Feature::try_from(geojson)?;

    let place = Place::try_from(feature)?;
    let place: domain::models::Place = place.into();

    print!("mdg body parsed as valid place | ");

    let worker = Agent::Worker;

    let result = domain::usecases::create_place(&worker, &place);
    match result {
        CreatePlaceResult::CantCreateAsUser => Err(Error::Error),
        CreatePlaceResult::DoCreate(p) => {
            use crate::db;
            // write db code here
            print!("inserting place in db | ");
            let mut conn = db::establish_connection()?;

            db::insert_place(&mut conn, p)?;

            // refresh http cache
            let cache_key = "places".to_string();
            let mut redis_conn = redis::establish_connection()?;
            redis::refresh_etag(&mut redis_conn, &cache_key)?;

            Ok(())
        }
    }?;
    println!("ok");

    Ok(())
}
