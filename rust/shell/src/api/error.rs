use crate::error::{Error, HttpError};

pub type ResponseError = rocket::response::status::Custom<String>;

impl From<Error> for ResponseError {
    fn from(value: Error) -> Self {
        match value {
            Error::AuthError(e) => rocket::response::status::Custom(
                rocket::http::Status::Unauthorized,
                format!("error: {:?}", e).to_string(),
            ),
            #[cfg(feature="rmqsub")]
            Error::GeoJSONSerdeError(e) => rocket::response::status::Custom(
                rocket::http::Status::InternalServerError,
                format!("error: {:?}", e).to_string(),
            ),
            Error::JwtError(e) => rocket::response::status::Custom(
                rocket::http::Status::Unauthorized,
                format!("error: {:?}", e).to_string(),
            ),
            Error::DotEnvyError(e) => rocket::response::status::Custom(
                rocket::http::Status::InternalServerError,
                format!("error: {:?}", e).to_string(),
            ), // map every adapters error to 500
            Error::StdEnvVarError(e) => rocket::response::status::Custom(
                rocket::http::Status::InternalServerError,
                format!("error: {:?}", e).to_string(),
            ), // map every adapters error to 500
            Error::DieselConnectionError(e) => rocket::response::status::Custom(
                rocket::http::Status::InternalServerError,
                format!("error: {:?}", e).to_string(),
            ), // map every adapters error to 500
            Error::DieselQueryError(e) => rocket::response::status::Custom(
                rocket::http::Status::InternalServerError,
                format!("error: {:?}", e).to_string(),
            ), // map every adapters error to 500
            #[cfg(feature="rmqsub")]
            Error::FromUTF8Error(e) => rocket::response::status::Custom(
                rocket::http::Status::InternalServerError,
                format!("error: {:?}", e).to_string(),
            ), // map every adapters error to 500
            #[cfg(feature="rmqsub")]
            Error::GeoJsonError(e) => rocket::response::status::Custom(
                rocket::http::Status::InternalServerError,
                format!("error: {:?}", e).to_string(),
            ), // map every adapters error to 500
            Error::HMacError(e) => rocket::response::status::Custom(
                rocket::http::Status::InternalServerError,
                format!("error: {:?}", e).to_string(),
            ), // map every adapters error to 500
            Error::R2D2Error(e) => rocket::response::status::Custom(
                rocket::http::Status::InternalServerError,
                format!("error: {:?}", e).to_string(),
            ), // map every adapters error to 500
            Error::RedisError(e) => rocket::response::status::Custom(
                rocket::http::Status::InternalServerError,
                format!("error: {:?}", e).to_string(),
            ), // map every adapters error to 500
            Error::LapinError(e) => rocket::response::status::Custom(
                rocket::http::Status::InternalServerError,
                format!("error: {:?}", e).to_string(),
            ), // map every adapters error to 500
            // map every adapters error to 500
            Error::SendError(e) => rocket::response::status::Custom(
                rocket::http::Status::InternalServerError,
                format!("error: {:?}", e).to_string(),
            ), // map every adapters error to 500
            Error::SerializeError(e) => rocket::response::status::Custom(
                rocket::http::Status::InternalServerError,
                format!("error: {:?}", e).to_string(),
            ), // map every adapters error to 500
            Error::HttpError(HttpError::NotFound) => rocket::response::status::Custom(
                rocket::http::Status::NotFound,
                "not found".to_string(),
            ),
            Error::HttpError(HttpError::Forbidden) => rocket::response::status::Custom(
                rocket::http::Status::Forbidden,
                "forbiddon".to_string(),
            ),
            Error::HttpError(HttpError::Conflict) => rocket::response::status::Custom(
                rocket::http::Status::Conflict,
                "conflict".to_string(),
            ),
            Error::HttpError(HttpError::Gone) => {
                rocket::response::status::Custom(rocket::http::Status::Gone, "gone".to_string())
            }
            Error::HttpError(HttpError::Unauthorized) => rocket::response::status::Custom(
                rocket::http::Status::Unauthorized,
                "unauthorized".to_string(),
            ),
            Error::HttpError(HttpError::NotModified) => rocket::response::status::Custom(
                rocket::http::Status::NotModified,
                "not modified".to_string(),
            ),
            Error::HttpError(HttpError::PreconditionFailed) => rocket::response::status::Custom(
                rocket::http::Status::PreconditionFailed,
                "precondition failed".to_string(),
            ),
            Error::HttpError(e) => rocket::response::status::Custom(
                rocket::http::Status::InternalServerError,
                format!("error: {:?}", e).to_string(),
            ),
            Error::Error => rocket::response::status::Custom(
                rocket::http::Status::InternalServerError,
                "error".to_string(),
            ),
        }
    }
}
