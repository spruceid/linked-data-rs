//! This library provides primitive traits to serialize and deserialize
//! Linked-Data types. It is shipped with derive macros (using the `derive`
//! feature) that can automatically implement those primitives for you.
//!
//! # Example
//!
//! ```
//! use iref::IriBuf;
//! use static_iref::iri;
//!
//! #[derive(linked_data::Serialize, linked_data::Deserialize)]
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
//! let quads = linked_data::to_quads(rdf_types::generator::Blank::new(), &value)
//!   .expect("RDF serialization failed");
//!
//! for quad in quads {
//!   use rdf_types::RdfDisplay;
//!   println!("{} .", quad.rdf_display())
//! }
//! ```
//!
//! This should print the following:
//! ```text
//! <http://example.org/JohnSmith> <http://example.org/name> "John Smith" .
//! <http://example.org/JohnSmith> <http://example.org/email> "john.smith@example.org" .
//! ```
#[doc(hidden)]
pub use rdf_types;

pub mod de;
mod r#impl;
pub mod ser;

pub use de::{DeserializeLinkedData, LinkedDataDeserializer};
pub use ser::{LinkedDataSerializer, SerializeLinkedData};
