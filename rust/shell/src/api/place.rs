use crate::auth::JwtIdentifiedSubject;
use crate::db::{self, find_user};
use crate::error::{Error, ResponseError};
use crate::redis::{self, match_etag, IfNoneMatchHeader};
use crate::rmq;
use crate::ServerState;
use domain::models::Agent;
use rocket::serde::json::Json;
use rocket::serde::{Deserialize, Serialize};
use rocket::State;

use super::EtagJson;

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
