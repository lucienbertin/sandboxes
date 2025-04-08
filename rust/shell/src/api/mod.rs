mod post;

pub use post::*;
use rocket::fairing::{Fairing, Info, Kind};
use rocket::http::{ContentType, Header};
use rocket::request::Request;
use rocket::response::Responder;
use rocket::serde::json::Json;
use rocket::Response;
use serde::Serialize;


/// Catches all OPTION requests in order to get the CORS related Fairing triggered.
#[options("/<_..>")]
pub fn all_options() {
    /* Intentionally left empty */
}

pub struct Cors;

#[rocket::async_trait]
impl Fairing for Cors {
    fn info(&self) -> Info {
        Info {
            name: "Cross-Origin-Resource-Sharing Fairing",
            kind: Kind::Response,
        }
    }

    async fn on_response<'r>(&self, _request: &'r Request<'_>, response: &mut Response<'r>) {
        response.set_header(Header::new("Access-Control-Allow-Origin", "*"));
        response.set_header(Header::new(
            "Access-Control-Allow-Methods",
            "POST, PATCH, PUT, DELETE, HEAD, OPTIONS, GET",
        ));
        response.set_header(Header::new("Access-Control-Allow-Headers", "*"));
        response.set_header(Header::new("Access-Control-Allow-Credentials", "true"));
    }
}

pub struct EtagJson<T>
where
    T: Serialize,
{
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
            Some(tag) => builder.header(Header {
                name: "ETag".to_string().into(),
                value: tag.into(),
            }),
            None => builder,
        };

        builder.ok()
    }
}
