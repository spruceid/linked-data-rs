//! This library provides primitive traits to serialize and deserialize
//! Linked-Data types. It is shipped with derive macros (using the `derive`
//! feature) that can automatically implement those primitives for you.
//!
//! # Example
//!
//! ```
//! use iref::IriBuf;
//! use serde_ld::SerializeLd;
//!
//! #[derive(SerializeLd)]
//! #[ld(prefix("ex" = "http://example.org/"))]
//! struct Foo {
//!   #[ld(id)]
//!   id: IriBuf,
//!
//!   #[ld("ex:name")]
//!   name: String,
//!
//!   #[ld("ex:email")]
//!   email: String
//! }
//! ```
#[cfg(feature = "derive")]
pub use serde_ld_derive::SerializeLd;

#[doc(hidden)]
pub use iref;

#[doc(hidden)]
pub use rdf_types;

mod datatypes;
mod quads;
mod ser;

pub use quads::{to_quads, to_quads_with, QuadSerializer};
pub use ser::*;
