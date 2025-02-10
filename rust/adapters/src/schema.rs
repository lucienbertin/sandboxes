// @generated automatically by Diesel CLI.

pub mod sql_types {
}

diesel::table! {
    use diesel::sql_types::*;
    use postgis_diesel::sql_types::*;

    posts (id) {
        id -> Int4,
        title -> Varchar,
        body -> Text,
        published -> Bool,
        geom -> Nullable<Geometry>,
    }
}
