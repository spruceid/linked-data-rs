//! This library provides primitive traits to serialize and deserialize
//! Linked-Data types. It is shipped with derive macros (using the `derive`
//! feature) that can automatically implement those primitives for you.
//!
//! # Example
//!
//! ```
//! use iref::IriBuf;
//! use linked_data::LinkedData;
//!
//! #[derive(LinkedData)]
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
pub use linked_data_derive::LinkedData;
use rdf_types::Vocabulary;

#[doc(hidden)]
pub use iref;

#[doc(hidden)]
pub use rdf_types;

mod anonymous;
mod datatypes;
mod graph;
mod lexical_representation;
mod predicate;
mod quads;
mod rdf;
mod subject;

pub use anonymous::*;
pub use graph::*;
pub use lexical_representation::*;
pub use predicate::*;
pub use quads::{to_quads, to_quads_with, QuadSerializer};
pub use rdf::*;
pub use subject::*;

/// Linked-Data type.
///
/// A Linked-Data type represents an RDF dataset which can be visited using the
/// [`visit`](Self::visit) method.
pub trait LinkedData<V: Vocabulary = (), I = ()> {
	/// Visit the RDF dataset represented by this type.
	fn visit<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: Visitor<V, I>;
}

impl<'a, V: Vocabulary, I, T: LinkedData<V, I>> LinkedData<V, I> for &'a T {
	fn visit<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: Visitor<V, I>,
	{
		T::visit(self, visitor)
	}
}

/// RDF dataset visitor.
pub trait Visitor<V: Vocabulary, I> {
	/// Type of the value returned by the visitor when the dataset has been
	/// entirely visited.
	type Ok;

	/// Error type.
	type Error;

	/// Visits the default graph of the dataset.
	fn default_graph<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataGraph<V, I>;

	/// Visits a named graph of the dataset.
	fn named_graph<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LexicalRepresentation<V, I> + LinkedDataGraph<V, I>;

	/// Ends the dataset visit.
	fn end(self) -> Result<Self::Ok, Self::Error>;
}

/// Any mutable reference to a visitor is itself a visitor.
impl<'s, V: Vocabulary, I, S: Visitor<V, I>> Visitor<V, I> for &'s mut S {
	type Ok = ();
	type Error = S::Error;

	fn default_graph<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataGraph<V, I>,
	{
		S::default_graph(self, value)
	}

	fn named_graph<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LexicalRepresentation<V, I> + LinkedDataGraph<V, I>,
	{
		S::named_graph(self, value)
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(())
	}
}
