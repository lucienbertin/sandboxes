#[derive(Debug)]
pub enum Error {
    DotEnvyError(dotenvy::Error),
    StdEnvVarError(std::env::VarError),
    DieselConnectionError(diesel::result::ConnectionError),
    DieselQueryError(diesel::result::Error),
    HMacError(hmac::digest::InvalidLength)
}
impl From<dotenvy::Error> for Error {
    fn from(value: dotenvy::Error) -> Self {
        Error::DotEnvyError(value)
    }
}
impl From<std::env::VarError> for Error {
    fn from(value: std::env::VarError) -> Self {
        Error::StdEnvVarError(value)
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
impl From<hmac::digest::InvalidLength> for Error {
    fn from(value: hmac::digest::InvalidLength) -> Self {
        Error::HMacError(value)
    }
}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("adapters error")
    }
}
impl std::error::Error for Error {}