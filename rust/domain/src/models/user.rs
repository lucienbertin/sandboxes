#[derive(PartialEq, Debug)]
pub struct User {
    pub id: i32,
    pub first_name: String,
    pub last_name: String,
    pub email: String,
    pub role: Role,
}

#[derive(PartialEq, Debug)]
pub enum Role {
    Admin,
    Writer,
    Reader,
}