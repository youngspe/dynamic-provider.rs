#![no_std]

extern crate maybe_borrow;

#[doc(hidden)]
pub mod lt;
mod provide;
mod query;
mod tag;
mod utils;

pub use lt::{Lt, WithLt};
pub use provide::{provide_by_ref_with_fn, provide_with_fn, when_provider, Provide, ProvideRef};
pub use query::{with_query, DynQuery as Query};
pub use tag::{TagFor, TypeTag};
