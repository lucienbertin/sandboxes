use geojson::PointType;

#[derive(PartialEq, Debug, Clone)]
pub struct Place {
    pub id: i32,
    pub name: String,
    pub point: PointType,
}
