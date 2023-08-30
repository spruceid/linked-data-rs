use iref::{Iri, IriBuf};
use rdf_types::{Interpretation, Vocabulary};

use crate::{FromLinkedDataError, Interpret, LinkedDataGraph, LinkedDataPredicateObjects};

/// Serialize a Linked-Data node.
pub trait LinkedDataSubject<V: Vocabulary = (), I: Interpretation = ()> {
	fn visit_subject<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<V, I>;
}

impl<'a, V: Vocabulary, I: Interpretation, T: ?Sized + LinkedDataSubject<V, I>>
	LinkedDataSubject<V, I> for &'a T
{
	fn visit_subject<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<V, I>,
	{
		T::visit_subject(self, serializer)
	}
}

impl<V: Vocabulary, I: Interpretation> LinkedDataSubject<V, I> for Iri {
	fn visit_subject<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<V, I>,
	{
		serializer.end()
	}
}

impl<V: Vocabulary, I: Interpretation> LinkedDataSubject<V, I> for IriBuf {
	fn visit_subject<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<V, I>,
	{
		serializer.end()
	}
}

pub trait SubjectVisitor<V: Vocabulary, I: Interpretation> {
	type Ok;
	type Error;

	/// Visit a predicate of the graph.
	fn predicate<L, T>(&mut self, predicate: &L, objects: &T) -> Result<(), Self::Error>
	where
		L: ?Sized + Interpret<V, I>,
		T: ?Sized + LinkedDataPredicateObjects<V, I>;

	fn graph<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataGraph<V, I>;

	fn end(self) -> Result<Self::Ok, Self::Error>;
}

impl<'s, V: Vocabulary, I: Interpretation, S: SubjectVisitor<V, I>> SubjectVisitor<V, I>
	for &'s mut S
{
	type Ok = ();
	type Error = S::Error;

	fn predicate<L, T>(&mut self, predicate: &L, objects: &T) -> Result<(), Self::Error>
	where
		L: ?Sized + Interpret<V, I>,
		T: ?Sized + LinkedDataPredicateObjects<V, I>,
	{
		S::predicate(self, predicate, objects)
	}

	fn graph<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataGraph<V, I>,
	{
		S::graph(self, value)
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(())
	}
}

pub trait LinkedDataDeserializeSubject<V: Vocabulary = (), I: Interpretation = ()>: Sized {
	fn deserialize_subject(
		vocabulary: &mut V,
		interpretation: &mut I,
		value: &impl LinkedDataSubject<V, I>,
	) -> Result<Self, FromLinkedDataError>;
}
