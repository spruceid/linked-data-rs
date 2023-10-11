use iref::{Iri, IriBuf};
use rdf_types::{
	BlankId, BlankIdBuf, BlankIdVocabularyMut, Id, Interpretation, IriVocabularyMut, Vocabulary,
};

use crate::{FromLinkedDataError, LinkedDataResource, LinkedDataSubject};

/// Type representing the objects of an RDF subject's predicate binding.
pub trait LinkedDataPredicateObjects<V: Vocabulary = (), I: Interpretation = ()> {
	fn visit_objects<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<V, I>;
}

impl<V: Vocabulary, I: Interpretation> LinkedDataPredicateObjects<V, I> for () {
	fn visit_objects<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<V, I>,
	{
		visitor.end()
	}
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

impl<V: Vocabulary, I: Interpretation> LinkedDataPredicateObjects<V, I> for BlankId
where
	V: BlankIdVocabularyMut,
{
	fn visit_objects<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<V, I>,
	{
		visitor.object(self)?;
		visitor.end()
	}
}

impl<V: Vocabulary, I: Interpretation> LinkedDataPredicateObjects<V, I> for BlankIdBuf
where
	V: BlankIdVocabularyMut,
{
	fn visit_objects<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<V, I>,
	{
		visitor.object(self)?;
		visitor.end()
	}
}

impl<V: Vocabulary, I: Interpretation, T, B> LinkedDataPredicateObjects<V, I> for Id<T, B>
where
	T: LinkedDataPredicateObjects<V, I>,
	B: LinkedDataPredicateObjects<V, I>,
{
	fn visit_objects<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<V, I>,
	{
		match self {
			Self::Iri(i) => i.visit_objects(visitor),
			Self::Blank(b) => b.visit_objects(visitor),
		}
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

pub trait LinkedDataDeserializePredicateObjects<V: Vocabulary = (), I: Interpretation = ()>:
	Sized
{
	fn deserialize_objects<'a, D>(
		vocabulary: &V,
		interpretation: &I,
		dataset: &D,
		graph: &D::Graph,
		objects: impl IntoIterator<Item = &'a I::Resource>,
	) -> Result<Self, FromLinkedDataError>
	where
		I::Resource: 'a,
		D: grdf::Dataset<
			Subject = I::Resource,
			Predicate = I::Resource,
			Object = I::Resource,
			GraphLabel = I::Resource,
		>;
}
