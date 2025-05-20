use crate::error::{Error, HttpError};
use rocket::request::Request;
use rocket::response::{self, Responder, Response};

#[rocket::async_trait]
impl<'r> Responder<'r, 'static> for Error {
    fn respond_to(self, _: &'r Request<'_>) -> response::Result<'static> {
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
            | Error::SerializeError(_) => rocket::http::Status::InternalServerError, // 500

            #[cfg(feature = "rmqsub")]
            Error::GeoJSONSerdeError(_)
            | Error::GeoJsonError(_)
            | Error::FromUTF8Error(_)
            | Error::Error => rocket::http::Status::InternalServerError, // 500

            Error::JwtError(_) | Error::AuthError(_) => rocket::http::Status::Unauthorized, // 401

            Error::HttpError(HttpError::Conflict) => rocket::http::Status::Conflict, // 409
            Error::HttpError(HttpError::Forbidden) => rocket::http::Status::Forbidden, // 403
            Error::HttpError(HttpError::Gone) => rocket::http::Status::Gone,         // 410
            Error::HttpError(HttpError::NotFound) => rocket::http::Status::NotFound, // 404
            Error::HttpError(HttpError::NotModified) => rocket::http::Status::NotModified, // 301
            Error::HttpError(HttpError::PreconditionFailed) => {
                rocket::http::Status::PreconditionFailed
            } // 412
            Error::HttpError(HttpError::Unauthorized) => rocket::http::Status::Unauthorized, // 401
            Error::HttpError(_) => rocket::http::Status::InternalServerError,        // 500
        };

        Response::build().status(status).ok()
    }
}
