// use serde::Deserialize;
// use geojson::{Feature, PointType};

// #[derive(Debug, Deserialize)]
// struct Place {
//     id: i32,
//     name: String,
// }

// #[derive(Debug, Deserialize)]
// struct PlaceGeoJSON  {
//     #[serde(deserialize_with = "deserialize_geometry")]
//     pub geometry: geo_types::Point<f64>,
//     pub properties: Place,

// }

use geojson::{Feature, PointType};

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
