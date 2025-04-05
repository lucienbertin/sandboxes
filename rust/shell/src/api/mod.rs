mod post;



pub use post::*;
use rocket::http::{ContentType, Header};
use rocket::response::Responder;
use rocket::request::Request;
use rocket::serde::json::Json;
use serde::Serialize;

pub struct EtagJson<T> where T: Serialize {
    body: Json<T>,
    etag: Option<String>,
}


#[rocket::async_trait]
impl<'r, T: Serialize> Responder<'r, 'static> for EtagJson<T> {
    fn respond_to(self, req: &'r Request<'_>) -> rocket::response::Result<'static> {
        let mut builder = rocket::response::Response::build_from(self.body.respond_to(req)?);
        let builder = builder
            .status(rocket::http::Status::Ok)
            .header(ContentType::JSON);
        let builder = match self.etag {
            Some(tag) => builder.header(Header { name: "ETag".to_string().into(), value: tag.into()}),
            None => builder
        };

        builder.ok()
    }
}