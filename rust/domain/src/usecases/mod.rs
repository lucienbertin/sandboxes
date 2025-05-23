mod consult_post;
mod consult_posts;
mod create_post;
mod delete_post;
mod edit_post;
mod publish_post;

mod consult_places;
mod create_place;

// flat re-export
pub use consult_post::*;
pub use consult_posts::*;
pub use create_post::*;
pub use delete_post::*;
pub use edit_post::*;
pub use publish_post::*;

pub use consult_places::*;
pub use create_place::*;
