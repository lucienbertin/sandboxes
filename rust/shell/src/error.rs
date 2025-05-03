#[derive(Debug)]
pub enum Error {
    DotEnvyError(dotenvy::Error),
    StdEnvVarError(std::env::VarError),

    #[cfg(feature="db")]
    DieselConnectionError(diesel::result::ConnectionError),
    #[cfg(feature="db")]
    DieselQueryError(diesel::result::Error),

    #[cfg(feature="api")]
    HMacError(hmac::digest::InvalidLength),
    #[cfg(feature="api")]
    JwtError(jwt::Error),
    #[cfg(feature="api")]
    AuthError(AuthError),
    #[cfg(feature="api")]
    HttpError(HttpError),

    #[cfg(any(feature="rmqpub", feature="rmqsub", feature="redis", feature="db"))]
    R2D2Error(r2d2::Error),

    #[cfg(any(feature="rmqpub", feature="rmqsub"))]
    LapinError(lapin::Error),
    #[cfg(feature="rmqsub")]
    GeoJsonError(geojson::Error),
    #[cfg(feature="rmqsub")]
    GeoJSONSerdeError(GeoJSONSerdeError),
    #[cfg(feature="rmqsub")]
    FromUTF8Error(std::string::FromUtf8Error),
    #[cfg(feature="rmqpub")]
    SendError(std::sync::mpsc::SendError<(String, String)>),
    
    #[cfg(feature="redis")]
    RedisError(redis::RedisError),

    #[cfg(any(feature="rmqpub", feature="api"))]
    SerializeError(serde_json::Error),

    Error, // Generic error
}
#[cfg(feature="api")]
impl From<AuthError> for Error {
    fn from(value: AuthError) -> Self {
        Error::AuthError(value)
    }
}
#[cfg(feature="api")]
impl From<HttpError> for Error {
    fn from(value: HttpError) -> Self {
        Error::HttpError(value)
    }
}
#[cfg(feature="rmqsub")]
impl From<GeoJSONSerdeError> for Error {
    fn from(value: GeoJSONSerdeError) -> Self {
        Error::GeoJSONSerdeError(value)
    }
}

#[cfg(feature="db")]
impl From<diesel::result::ConnectionError> for Error {
    fn from(value: diesel::result::ConnectionError) -> Self {
        Error::DieselConnectionError(value)
    }
}
#[cfg(feature="db")]
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
#[cfg(feature="rmqsub")]
impl From<std::string::FromUtf8Error> for Error {
    fn from(value: std::string::FromUtf8Error) -> Self {
        Error::FromUTF8Error(value)
    }
}
#[cfg(feature="rmqsub")]
impl From<geojson::Error> for Error {
    fn from(value: geojson::Error) -> Self {
        Error::GeoJsonError(value)
    }
}
#[cfg(feature="api")]
impl From<hmac::digest::InvalidLength> for Error {
    fn from(value: hmac::digest::InvalidLength) -> Self {
        Error::HMacError(value)
    }
}
#[cfg(feature="api")]
impl From<jwt::Error> for Error {
    fn from(value: jwt::Error) -> Self {
        Error::JwtError(value)
    }
}
#[cfg(any(feature="rmqpub", feature="rmqsub"))]
impl From<lapin::Error> for Error {
    fn from(value: lapin::Error) -> Self {
        Error::LapinError(value)
    }
}
#[cfg(any(feature="rmqpub", feature="rmqsub", feature="redis", feature="db"))]
impl From<r2d2::Error> for Error {
    fn from(value: r2d2::Error) -> Self {
        Error::R2D2Error(value)
    }
}
#[cfg(feature="redis")]
impl From<redis::RedisError> for Error {
    fn from(value: redis::RedisError) -> Self {
        Error::RedisError(value)
    }
}
#[cfg(feature="rmqpub")]
impl From<std::sync::mpsc::SendError<(String, String)>> for Error {
    fn from(value: std::sync::mpsc::SendError<(String, String)>) -> Self {
        Error::SendError(value)
    }
}
#[cfg(any(feature="rmqpub", feature="api"))]
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
        match &self {
            Error::DotEnvyError(error) => f.write_str(format!("shell error {:?}", error).as_str()),
            Error::StdEnvVarError(error) => f.write_str(format!("shell error {:?}", error).as_str()),

            #[cfg(feature="db")]
            Error::DieselConnectionError(error) => f.write_str(format!("shell error {:?}", error).as_str()),
            #[cfg(feature="db")]
            Error::DieselQueryError(error) => f.write_str(format!("shell error {:?}", error).as_str()),

            #[cfg(feature="api")]
            Error::HMacError(error) => f.write_str(format!("shell error {:?}", error).as_str()),
            #[cfg(feature="api")]
            Error::JwtError(error) => f.write_str(format!("shell error {:?}", error).as_str()),
            #[cfg(feature="api")]
            Error::AuthError(error) => f.write_str(format!("shell error {:?}", error).as_str()),
            #[cfg(feature="api")]
            Error::HttpError(error) => f.write_str(format!("shell error {:?}", error).as_str()),

            #[cfg(any(feature="rmqpub", feature="rmqsub", feature="redis", feature="db"))]
            Error::R2D2Error(error) => f.write_str(format!("shell error {:?}", error).as_str()),
            
            #[cfg(any(feature="rmqpub", feature="rmqsub"))]
            Error::LapinError(error) => f.write_str(format!("shell error {:?}", error).as_str()),
            #[cfg(feature="rmqsub")]
            Error::GeoJsonError(error) => f.write_str(format!("shell error {:?}", error).as_str()),
            #[cfg(feature="rmqsub")]
            Error::GeoJSONSerdeError(error) => f.write_str(format!("shell error {:?}", error).as_str()),
            #[cfg(feature="rmqsub")]
            Error::FromUTF8Error(error) => f.write_str(format!("shell error {:?}", error).as_str()),
            #[cfg(feature="rmqpub")]
            Error::SendError(error) => f.write_str(format!("shell error {:?}", error).as_str()),

            #[cfg(feature="redis")]
            Error::RedisError(error) => f.write_str(format!("shell error {:?}", error).as_str()),

            #[cfg(any(feature="rmqpub", feature="api"))]
            Error::SerializeError(error) => f.write_str(format!("shell error {:?}", error).as_str()),


            Error::Error => f.write_str("shell error"),
        }
    }
}
impl std::error::Error for Error {}

#[cfg(feature="api")]
#[derive(Debug)]
pub enum AuthError {
    NoAuthorizationHeader,
    NoBearerToken,
    NoSubjectClaim,
}

#[cfg(feature="api")]
#[derive(Debug)]
pub enum HttpError {
    NoIfMatchHeader,
    NoIfNoneMatchHeader,
    NotFound,
    Forbidden,
    Unauthorized,
    Conflict,
    Gone,
    NotModified,
    PreconditionFailed,
}

#[derive(Debug, )]
pub enum GeoJSONSerdeError {
    NoProperties,
    NoGeometry,
    InvalidGeometryType,
    MissingProperty(String),
    InvalidPropertyType(String),
}
impl std::fmt::Display for GeoJSONSerdeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self {
            GeoJSONSerdeError::NoProperties => write!(f, "missing 'properties' field"),
            GeoJSONSerdeError::NoGeometry => write!(f, "missing 'geometry' field"),
            GeoJSONSerdeError::InvalidGeometryType => write!(f, "invalid geometry type"),
            GeoJSONSerdeError::MissingProperty(p) => write!(f, "missing required property '{}'", p),
            GeoJSONSerdeError::InvalidPropertyType(p) => write!(f, "invalid type for property '{}'", p),
        }
    }
}