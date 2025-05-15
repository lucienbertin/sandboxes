use crate::error::Error;

#[cfg(feature = "db")]
use crate::db;

use leptos::prelude::*;
use leptos_meta::{provide_meta_context, Stylesheet, Title};
use leptos_router::{
    components::{Route, Router, Routes},
    StaticSegment,
};

#[cfg(feature = "appssr")]
pub fn shell(options: LeptosOptions) -> impl IntoView {
    use leptos_meta::MetaTags;
    view! {
        <!DOCTYPE html>
        <html lang="en">
            <head>
                <meta charset="utf-8"/>
                <meta name="viewport" content="width=device-width, initial-scale=1"/>
                <AutoReload options=options.clone() />
                <HydrationScripts options/>
                <MetaTags/>
            </head>
            <body>
                <App/>
            </body>
        </html>
    }
}

#[component]
pub fn App() -> impl IntoView {
    // Provides context that manages stylesheets, titles, meta tags, etc.
    provide_meta_context();

    view! {
        // injects a stylesheet into the document <head>
        // id=leptos means cargo-leptos will hot-reload this stylesheet
        <Stylesheet id="leptos" href="/pkg/leptos-axum.css"/>

        // sets the document title
        <Title text="Welcome to Leptos"/>

        // content for this welcome page
        <Router>
            <main>
                <Routes fallback=|| "Page not found.".into_view()>
                    <Route path=StaticSegment("") view=HomePage/>
                </Routes>
            </main>
        </Router>
    }
}

/// Renders the home page of your application.
#[component]
fn HomePage() -> impl IntoView {
    // use leptos::task::spawn_local;
    // Creates a reactive value to update the button
    // let count = RwSignal::new(0);
    let (count, set_count) = signal(0);
    let posts = Resource::new(
        move || count.get(),
        |_| async move { get_posts().await.expect("AAAAAA") },
    ); // EXPECT !!!!!!
    let incr = move |_| set_count.update(|c| *c += 1);

    view! {
        <h1>"Welcome to Leptos!"</h1>
        <button on:click=incr>"Click Me: " {count}</button>
        <Suspense
            fallback=move || view! { <p>"Loading..."</p> }
        >
            {move || {
                posts.get().map(|posts| view! {
                    <ul>
                        {posts.into_iter().map(|p| view! { <li> {p.title} </li> } ).collect_view() }
                    </ul>
                })
        }}
        </Suspense>
    }
}

#[server]
pub async fn get_posts() -> Result<Vec<Post>, ServerFnError> {
    let db_pool = use_context::<db::DbPool>().expect("no db pool"); // EXPECT!!!
    let mut db_conn = db::get_conn(&db_pool).map_err(|e| ServerFnError::WrappedServerError(e))?;

    let posts = db_conn
        .build_transaction()
        .read_only()
        .run(|conn| -> Result<Vec<domain::models::Post>, Error> { db::select_posts(conn) })
        .map_err(|e| ServerFnError::WrappedServerError(e))?;
    let posts = posts.into_iter().map(|x| x.into()).collect();

    Ok(posts)
}

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone)]
pub struct User {
    // pub id: i32,
    pub first_name: String,
    pub last_name: String,
    pub email: String,
}
#[derive(Serialize, Deserialize, Clone)]
pub struct Post {
    pub id: i32,
    pub title: String,
    pub body: String,
    pub published: bool,
    pub author: User,
}

impl From<domain::models::User> for User {
    fn from(value: domain::models::User) -> Self {
        Self {
            email: value.email,
            first_name: value.first_name,
            last_name: value.last_name,
        }
    }
}

impl From<domain::models::Post> for Post {
    fn from(value: domain::models::Post) -> Self {
        Self {
            id: value.id,
            title: value.title,
            body: value.body,
            published: value.published,
            author: value.author.into(),
        }
    }
}
