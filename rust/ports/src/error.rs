#[derive(Debug)]
pub enum Error {
    AuthError(AuthError),
    JwtError(jwt::Error),
    AdaptersError(adapters::error::Error)
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
pub enum  AuthError {
    NoAuthorizationHeader,
    NoBearerToken,
    NoSubjectClaim,
}