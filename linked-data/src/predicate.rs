use iref::{Iri, IriBuf};
use rdf_types::{
	interpretation::{ReverseBlankIdInterpretation, ReverseIriInterpretation},
	BlankId, BlankIdBuf, BlankIdVocabularyMut, Id, Interpretation, IriVocabularyMut,
	ReverseIdInterpretation, Vocabulary,
};

use crate::{
	FromLinkedDataError, LinkedDataDeserializeSubject, LinkedDataResource, LinkedDataSubject,
};

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

macro_rules! deserialize_single_object {
	() => {
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
			>,
		{
			use crate::LinkedDataDeserializeSubject;
			let mut objects = objects.into_iter();
			match objects.next() {
				Some(object) => {
					if objects.next().is_none() {
						Self::deserialize_subject(
							vocabulary,
							interpretation,
							dataset,
							graph,
							object,
						)
					} else {
						Err(FromLinkedDataError::TooManyValues)
					}
				}
				None => Err(FromLinkedDataError::MissingRequiredValue),
			}
		}
	};
}

impl<V: Vocabulary, I: Interpretation> LinkedDataDeserializePredicateObjects<V, I> for IriBuf
where
	I: ReverseIriInterpretation<Iri = V::Iri>,
{
	deserialize_single_object!();
}

impl<V: Vocabulary, I: Interpretation> LinkedDataDeserializePredicateObjects<V, I> for BlankIdBuf
where
	I: ReverseBlankIdInterpretation<BlankId = V::BlankId>,
{
	deserialize_single_object!();
}

impl<V: Vocabulary, I: Interpretation> LinkedDataDeserializePredicateObjects<V, I> for Id
where
	I: ReverseIdInterpretation<Iri = V::Iri, BlankId = V::BlankId>,
{
	deserialize_single_object!();
}

impl<V: Vocabulary, I: Interpretation, T: LinkedDataDeserializePredicateObjects<V, I>>
	LinkedDataDeserializePredicateObjects<V, I> for Box<T>
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
		>,
	{
		T::deserialize_objects(vocabulary, interpretation, dataset, graph, objects).map(Box::new)
	}
}

impl<V: Vocabulary, I: Interpretation, T: LinkedDataDeserializeSubject<V, I>>
	LinkedDataDeserializePredicateObjects<V, I> for Option<T>
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
		>,
	{
		let mut objects = objects.into_iter();
		match objects.next() {
			Some(object) => {
				if objects.next().is_none() {
					T::deserialize_subject(vocabulary, interpretation, dataset, graph, object)
						.map(Some)
				} else {
					Err(FromLinkedDataError::TooManyValues)
				}
			}
			None => Ok(None),
		}
	}
}
