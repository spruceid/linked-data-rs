use iref::{Iri, IriBuf};
use rdf_types::{BlankId, BlankIdBuf, Id, Interpretation, Vocabulary};

use crate::{FromLinkedDataError, LinkedDataGraph, LinkedDataPredicateObjects, LinkedDataResource};

/// Serialize a Linked-Data node.
pub trait LinkedDataSubject<V: Vocabulary = (), I: Interpretation = ()> {
	fn visit_subject<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<V, I>;
}

impl<V: Vocabulary, I: Interpretation> LinkedDataSubject<V, I> for () {
	fn visit_subject<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<V, I>,
	{
		serializer.end()
	}
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

impl<V: Vocabulary, I: Interpretation, T: ?Sized + LinkedDataSubject<V, I>> LinkedDataSubject<V, I>
	for Box<T>
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

impl<V: Vocabulary, I: Interpretation> LinkedDataSubject<V, I> for BlankId {
	fn visit_subject<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<V, I>,
	{
		serializer.end()
	}
}

impl<V: Vocabulary, I: Interpretation> LinkedDataSubject<V, I> for BlankIdBuf {
	fn visit_subject<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<V, I>,
	{
		serializer.end()
	}
}

impl<V: Vocabulary, I: Interpretation, T, B> LinkedDataSubject<V, I> for Id<T, B>
where
	T: LinkedDataSubject<V, I>,
	B: LinkedDataSubject<V, I>,
{
	fn visit_subject<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<V, I>,
	{
		match self {
			Self::Iri(i) => i.visit_subject(serializer),
			Self::Blank(b) => b.visit_subject(serializer),
		}
	}
}

pub trait SubjectVisitor<V: Vocabulary, I: Interpretation> {
	type Ok;
	type Error;

	/// Visit a predicate of the graph.
	fn predicate<L, T>(&mut self, predicate: &L, objects: &T) -> Result<(), Self::Error>
	where
		L: ?Sized + LinkedDataResource<V, I>,
		T: ?Sized + LinkedDataPredicateObjects<V, I>;

	/// Visit a reverse predicate of the graph.
	fn reverse_predicate<L, T>(&mut self, predicate: &L, subjects: &T) -> Result<(), Self::Error>
	where
		L: ?Sized + LinkedDataResource<V, I>,
		T: ?Sized + LinkedDataPredicateObjects<V, I>;

	fn graph<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataGraph<V, I>;

	fn include<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataResource<V, I> + LinkedDataSubject<V, I>;

	fn end(self) -> Result<Self::Ok, Self::Error>;
}

impl<'s, V: Vocabulary, I: Interpretation, S: SubjectVisitor<V, I>> SubjectVisitor<V, I>
	for &'s mut S
{
	type Ok = ();
	type Error = S::Error;

	fn predicate<L, T>(&mut self, predicate: &L, objects: &T) -> Result<(), Self::Error>
	where
		L: ?Sized + LinkedDataResource<V, I>,
		T: ?Sized + LinkedDataPredicateObjects<V, I>,
	{
		S::predicate(self, predicate, objects)
	}

	fn reverse_predicate<L, T>(&mut self, predicate: &L, subjects: &T) -> Result<(), Self::Error>
	where
		L: ?Sized + LinkedDataResource<V, I>,
		T: ?Sized + LinkedDataPredicateObjects<V, I>,
	{
		S::reverse_predicate(self, predicate, subjects)
	}

	fn graph<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataGraph<V, I>,
	{
		S::graph(self, value)
	}

	fn include<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataResource<V, I> + LinkedDataSubject<V, I>,
	{
		S::include(self, value)
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(())
	}
}

pub trait LinkedDataDeserializeSubject<V: Vocabulary = (), I: Interpretation = ()>: Sized {
	fn deserialize_subject<D>(
		vocabulary: &V,
		interpretation: &I,
		dataset: &D,
		graph: &D::Graph,
		resource: &I::Resource,
	) -> Result<Self, FromLinkedDataError>
	where
		D: grdf::Dataset<
			Subject = I::Resource,
			Predicate = I::Resource,
			Object = I::Resource,
			GraphLabel = I::Resource,
		>;
}
