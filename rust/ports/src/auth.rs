use jwt::{SignWithKey, VerifyWithKey};
use rocket::{http::Status, request::{FromRequest, Outcome}, Request};
use std::collections::BTreeMap;
use super::error::Error;

use adapters::load_hmac_key;

pub fn _sign_token(sub: String) -> Result<String, Error> {
    let key = load_hmac_key()?;

    let mut claims = BTreeMap::new();
    claims.insert("sub", sub);
    let token_str = claims.sign_with_key(&key)?;

    Ok(token_str)
}

pub fn verify_token(token_str: &str) -> Result<BTreeMap<String, String>, Error> {
    let key = load_hmac_key()?;

    let claims: BTreeMap<String, String> = token_str.verify_with_key(&key)?;

    Ok(claims)
}

#[derive(Debug)]
pub struct JwtIdentifiedSubject {
    pub email: String,
}

fn extract_subject(req: &Request<'_>) -> Result<JwtIdentifiedSubject, Error> {
    use super::error::AuthError;
    let result = req.headers().get_one("Authorization").ok_or(Error::AuthError(AuthError::NoAuthorizationHeader))?;
    let result = 
        if result.starts_with("Bearer ") {
            Ok(&result[7..])
        } else {
            Err(Error::AuthError(AuthError::NoBearerToken))
        }?;
    let result = verify_token(result)?;

    let result = result.get("sub").ok_or(Error::AuthError(AuthError::NoSubjectClaim))?;
    let result = JwtIdentifiedSubject{
        email: result.to_string(),
    };

    Ok(result)
}

#[rocket::async_trait]
impl<'r> FromRequest<'r> for JwtIdentifiedSubject {
    type Error = super::error::Error;

    async fn from_request(req: &'r Request<'_>) -> Outcome<Self, Self::Error> {
        let result = extract_subject(req);

        match result {
            Ok(subject) => Outcome::Success(subject),
            Err(e) => Outcome::Error((Status::Unauthorized, e))
        }
    }
}

#[test]
fn test_sign_token() {
    let subject = "john@d.oe";
    let result = _sign_token(subject.into());

    match result {
        Ok(token) => println!("token: {}", token),
        Err(e) => println!("error {:?}", e)
    };
}#[test]
fn test_verify_token() {
    let token_str = "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJsdWNpZW5AYmVydC5pbiJ9.MQ2AtPRuMhZuu84jFpjbnZF3tMREpSi51YEU6yq8KBI";
    let result = verify_token(token_str);

    match result {
        Ok(claims) => println!("token verified, claims: {:?}", claims),
        Err(e) => println!("error {:?}", e)
    };
}