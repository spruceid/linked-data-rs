//! This library provides primitive traits to serialize and deserialize
//! Linked-Data types. It is shipped with derive macros (using the `derive`
//! feature) that can automatically implement those primitives for you.
//!
//! # Example
//!
//! ```ignore
//! use rdf_syntax::{IriBuf, iri};
//! use linked_data::{SerializeLinkedData, DeserializeLinkedData};
//!
//! #[derive(SerializeLinkedData, DeserializeLinkedData)]
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
//!
//! let value = Foo {
//!   id: iri!("http://example.org/JohnSmith").to_owned(),
//!   name: "John Smith".to_owned(),
//!   email: "john.smith@example.org".to_owned()
//! };
//!
//! let quads = linked_data::ser::to_rdf_quads(&value)
//!   .expect("RDF serialization failed");
//!
//! for quad in quads {
//!   println!("{quad} .")
//! }
//! ```
//!
//! This should print the following:
//! ```text
//! <http://example.org/JohnSmith> <http://example.org/name> "John Smith" .
//! <http://example.org/JohnSmith> <http://example.org/email> "john.smith@example.org" .
//! ```
#[doc(hidden)]
pub use rdf_syntax;

#[cfg(feature = "derive")]
pub use linked_data_derive::{DeserializeLinkedData, SerializeLinkedData};

pub mod de;
mod r#impl;
pub mod ser;

pub use de::{DeserializeLinkedData, LinkedDataDeserializer};
pub use ser::{LinkedDataSerializer, SerializeLinkedData};

/// Utility wrapper to turn lists into unordered collections.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct RdfUnordered<T>(pub T);
