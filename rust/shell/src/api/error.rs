use crate::error::{Error, HttpError};
use rocket::request::Request;
use rocket::response::{self, Responder, Response};

#[rocket::async_trait]
impl<'r> Responder<'r, 'static> for Error {
    fn respond_to(self, _: &'r Request<'_>) -> response::Result<'static> {
        use rocket::http::Status;
        let status = match self {
            Error::DotEnvyError(_)
            | Error::StdEnvVarError(_)
            | Error::DieselConnectionError(_)
            | Error::DieselQueryError(_)
            | Error::HMacError(_)
            | Error::R2D2Error(_)
            | Error::LapinError(_)
            | Error::SendError(_)
            | Error::RedisError(_)
            | Error::SerializeError(_) => Status::InternalServerError, // 500

            #[cfg(feature = "rmqsub")]
            Error::GeoJSONSerdeError(_)
            | Error::GeoJsonError(_)
            | Error::FromUTF8Error(_)
            | Error::Error => Status::InternalServerError, // 500

            Error::JwtError(_) | Error::AuthError(_) => Status::Unauthorized, // 401

            Error::HttpError(HttpError::NotModified) => Status::NotModified, // 301

            Error::HttpError(HttpError::Unauthorized) => Status::Unauthorized, // 401
            Error::HttpError(HttpError::Forbidden) => Status::Forbidden,       // 403
            Error::HttpError(HttpError::NotFound) => Status::NotFound,         // 404
            Error::HttpError(HttpError::Conflict) => Status::Conflict,         // 409
            Error::HttpError(HttpError::Gone) => Status::Gone,                 // 410
            Error::HttpError(HttpError::PreconditionFailed) => Status::PreconditionFailed, // 412

            Error::HttpError(_) => Status::InternalServerError, // 500
        };

        Response::build().status(status).ok()
    }
}
