use iref::{Iri, IriBuf};
use rdf_types::{Interpretation, IriVocabularyMut, Vocabulary};

use crate::{LinkedDataResource, LinkedDataSubject};

/// Type representing the objects of an RDF subject's predicate binding.
pub trait LinkedDataPredicateObjects<V: Vocabulary = (), I: Interpretation = ()> {
	fn visit_objects<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<V, I>;
}

impl<'a, V: Vocabulary, I: Interpretation, T: ?Sized + LinkedDataPredicateObjects<V, I>>
	LinkedDataPredicateObjects<V, I> for &'a T
{
	fn visit_objects<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<V, I>,
	{
		T::visit_objects(self, visitor)
	}
}

impl<V: Vocabulary, I: Interpretation, T: ?Sized + LinkedDataPredicateObjects<V, I>>
	LinkedDataPredicateObjects<V, I> for Box<T>
{
	fn visit_objects<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<V, I>,
	{
		T::visit_objects(self, visitor)
	}
}

impl<V: Vocabulary, I: Interpretation, T: LinkedDataSubject<V, I> + LinkedDataResource<V, I>>
	LinkedDataPredicateObjects<V, I> for Option<T>
{
	fn visit_objects<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<V, I>,
	{
		if let Some(t) = self {
			visitor.object(t)?;
		}

		visitor.end()
	}
}

impl<V: Vocabulary, I: Interpretation, T: LinkedDataSubject<V, I> + LinkedDataResource<V, I>>
	LinkedDataPredicateObjects<V, I> for [T]
{
	fn visit_objects<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<V, I>,
	{
		for t in self {
			visitor.object(t)?;
		}

		visitor.end()
	}
}

impl<V: Vocabulary, I: Interpretation, T: LinkedDataSubject<V, I> + LinkedDataResource<V, I>>
	LinkedDataPredicateObjects<V, I> for Vec<T>
{
	fn visit_objects<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<V, I>,
	{
		for t in self {
			visitor.object(t)?;
		}

		visitor.end()
	}
}

impl<V: Vocabulary, I: Interpretation> LinkedDataPredicateObjects<V, I> for Iri
where
	V: IriVocabularyMut,
{
	fn visit_objects<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<V, I>,
	{
		visitor.object(self)?;
		visitor.end()
	}
}

impl<V: Vocabulary, I: Interpretation> LinkedDataPredicateObjects<V, I> for IriBuf
where
	V: IriVocabularyMut,
{
	fn visit_objects<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<V, I>,
	{
		visitor.object(self)?;
		visitor.end()
	}
}

pub trait PredicateObjectsVisitor<V: Vocabulary, I: Interpretation> {
	type Ok;
	type Error;

	fn object<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataResource<V, I> + LinkedDataSubject<V, I>;

	fn end(self) -> Result<Self::Ok, Self::Error>;
}
