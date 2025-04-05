mod post;

pub use post::*;
use rocket::http::{ContentType, Header};
use rocket::request::Request;
use rocket::response::Responder;
use rocket::serde::json::Json;
use serde::Serialize;

pub struct EtagJson<T>
where
    T: Serialize,
{
    body: Json<T>,
    etag: String,
}

#[rocket::async_trait]
impl<'r, T: Serialize> Responder<'r, 'static> for EtagJson<T> {
    fn respond_to(self, req: &'r Request<'_>) -> rocket::response::Result<'static> {
        rocket::response::Response::build_from(self.body.respond_to(req)?)
            .status(rocket::http::Status::Ok)
            .header(ContentType::JSON)
            .header(Header {
                name: "ETag".to_string().into(),
                value: self.etag.into(),
            })
            .ok()
    }
}
