#[derive(Debug)]
pub enum Error {
    DieselConnectionError(diesel::result::ConnectionError),
    DieselQueryError(diesel::result::Error),
    DotEnvyError(dotenvy::Error),
    HMacError(hmac::digest::InvalidLength),
    JwtError(jwt::Error),
    LapinError(lapin::Error),
    R2D2Error(r2d2::Error),
    RedisError(redis::RedisError),
    SendError(std::sync::mpsc::SendError<(String, String)>),
    SerializeError(serde_json::Error),
    StdEnvVarError(std::env::VarError),

    AuthError(AuthError),
    HttpError(HttpError),
    GeoJSONSerdeError(GeoJSONSerdeError),

    NotFound,
    Forbidden,
    Unauthorized,
    Conflict,
    Gone,
    NotModified,
    PreconditionFailed,
}
impl From<AuthError> for Error {
    fn from(value: AuthError) -> Self {
        Error::AuthError(value)
    }
}
impl From<HttpError> for Error {
    fn from(value: HttpError) -> Self {
        Error::HttpError(value)
    }
}
impl From<GeoJSONSerdeError> for Error {
    fn from(value: GeoJSONSerdeError) -> Self {
        Error::GeoJSONSerdeError(value)
    }
}
impl From<diesel::result::ConnectionError> for Error {
    fn from(value: diesel::result::ConnectionError) -> Self {
        Error::DieselConnectionError(value)
    }
}
impl From<diesel::result::Error> for Error {
    fn from(value: diesel::result::Error) -> Self {
        Error::DieselQueryError(value)
    }
}
impl From<dotenvy::Error> for Error {
    fn from(value: dotenvy::Error) -> Self {
        Error::DotEnvyError(value)
    }
}
impl From<hmac::digest::InvalidLength> for Error {
    fn from(value: hmac::digest::InvalidLength) -> Self {
        Error::HMacError(value)
    }
}
impl From<jwt::Error> for Error {
    fn from(value: jwt::Error) -> Self {
        Error::JwtError(value)
    }
}
impl From<lapin::Error> for Error {
    fn from(value: lapin::Error) -> Self {
        Error::LapinError(value)
    }
}
impl From<r2d2::Error> for Error {
    fn from(value: r2d2::Error) -> Self {
        Error::R2D2Error(value)
    }
}
impl From<redis::RedisError> for Error {
    fn from(value: redis::RedisError) -> Self {
        Error::RedisError(value)
    }
}
impl From<std::sync::mpsc::SendError<(String, String)>> for Error {
    fn from(value: std::sync::mpsc::SendError<(String, String)>) -> Self {
        Error::SendError(value)
    }
}
impl From<serde_json::Error> for Error {
    fn from(value: serde_json::Error) -> Self {
        Error::SerializeError(value)
    }
}
impl From<std::env::VarError> for Error {
    fn from(value: std::env::VarError) -> Self {
        Error::StdEnvVarError(value)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("shell error")
    }
}
impl std::error::Error for Error {}

#[derive(Debug)]
pub enum AuthError {
    NoAuthorizationHeader,
    NoBearerToken,
    NoSubjectClaim,
}

#[derive(Debug)]
pub enum HttpError {
    NoIfMatchHeader,
    NoIfNoneMatchHeader,
}
#[derive(Debug)]
pub enum GeoJSONSerdeError {
    NoProperties,
    NoGeometry,
    InvalidGeometryType,
    MissingProperty(String),
    InvalidPropertyType(String),
}
pub type ResponseError = rocket::response::status::Custom<String>;

impl From<Error> for ResponseError {
    fn from(value: Error) -> Self {
        match value {
            Error::AuthError(e) => rocket::response::status::Custom(
                rocket::http::Status::Unauthorized,
                format!("error: {:?}", e).to_string(),
            ),
            Error::HttpError(e) => rocket::response::status::Custom(
                rocket::http::Status::InternalServerError,
                format!("error: {:?}", e).to_string(),
            ),
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
            Error::NotFound => rocket::response::status::Custom(
                rocket::http::Status::NotFound,
                "not found".to_string(),
            ),
            Error::Forbidden => rocket::response::status::Custom(
                rocket::http::Status::Forbidden,
                "forbiddon".to_string(),
            ),
            Error::Conflict => rocket::response::status::Custom(
                rocket::http::Status::Conflict,
                "conflict".to_string(),
            ),
            Error::Gone => {
                rocket::response::status::Custom(rocket::http::Status::Gone, "gone".to_string())
            }
            Error::Unauthorized => rocket::response::status::Custom(
                rocket::http::Status::Unauthorized,
                "unauthorized".to_string(),
            ),
            Error::NotModified => rocket::response::status::Custom(
                rocket::http::Status::NotModified,
                "not modified".to_string(),
            ),
            Error::PreconditionFailed => rocket::response::status::Custom(
                rocket::http::Status::PreconditionFailed,
                "precondition failed".to_string(),
            ),
        }
    }
}
