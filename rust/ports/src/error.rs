#[derive(Debug)]
pub enum Error {
    AuthError(AuthError),
    JwtError(jwt::Error),
    AdaptersError(adapters::error::Error),
}
impl From<AuthError> for Error {
    fn from(value: AuthError) -> Self {
        Error::AuthError(value)
    }
}
impl From<jwt::Error> for Error {
    fn from(value: jwt::Error) -> Self {
        Error::JwtError(value)
    }
}
impl From<adapters::error::Error> for Error {
    fn from(value: adapters::error::Error) -> Self {
        Error::AdaptersError(value)
    }
}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("ports error")
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
            Error::AdaptersError(e) => rocket::response::status::Custom(
                rocket::http::Status::InternalServerError,
                format!("error: {:?}", e).to_string(),
            ), // map every adapters error to 500
            Error::AuthError(e) => rocket::response::status::Custom(
                rocket::http::Status::Unauthorized,
                format!("error: {:?}", e).to_string(),
            ),
            Error::JwtError(e) => rocket::response::status::Custom(
                rocket::http::Status::Unauthorized,
                format!("error: {:?}", e).to_string(),
            ),
        }
    }
}
