mod error;

#[cfg(feature = "db")]
mod db;

#[cfg(any(feature = "appcsr"))]
pub mod app;

#[cfg(feature = "appcsr")]
#[wasm_bindgen::prelude::wasm_bindgen]
pub fn hydrate() {
    use crate::app::*;
    console_error_panic_hook::set_once();
    leptos::mount::hydrate_body(App);
}
