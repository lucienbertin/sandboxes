#[derive(Debug)]
pub enum Error {
    AuthError(AuthError),
    DieselConnectionError(diesel::result::ConnectionError),
    DieselQueryError(diesel::result::Error),
    DotEnvyError(dotenvy::Error),
    HMacError(hmac::digest::InvalidLength),
    JwtError(jwt::Error),
    LapinError(lapin::Error),
    R2D2Error(r2d2::Error),
    StdEnvVarError(std::env::VarError),

    NotFound,
    Forbidden,
    Unauthorized,
    Conflict,
    Gone,
}
impl From<AuthError> for Error {
    fn from(value: AuthError) -> Self {
        Error::AuthError(value)
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

impl From<Error> for rocket::response::status::Custom<String> {
    fn from(value: Error) -> Self {
        match value {
            Error::AuthError(e) => rocket::response::status::Custom(
                rocket::http::Status::Unauthorized,
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
            Error::LapinError(e) => rocket::response::status::Custom(
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
        }
    }
}
