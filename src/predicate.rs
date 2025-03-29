use iref::{Iri, IriBuf};
use rdf_types::{
	dataset::PatternMatchingDataset,
	interpretation::{
		ReverseBlankIdInterpretation, ReverseIdInterpretation, ReverseIriInterpretation,
	},
	vocabulary::{BlankIdVocabularyMut, IriVocabularyMut},
	BlankId, BlankIdBuf, Id, Interpretation, Vocabulary,
};

use crate::{
	Context, FromLinkedDataError, LinkedDataDeserializeSubject, LinkedDataResource,
	LinkedDataSubject,
};

/// Type representing the objects of an RDF subject's predicate binding.
pub trait LinkedDataPredicateObjects<I: Interpretation = (), V: Vocabulary = ()> {
	fn accept_objects_visitor<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<I, V>;
}

impl<I: Interpretation, V: Vocabulary> LinkedDataPredicateObjects<I, V> for () {
	fn accept_objects_visitor<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<I, V>,
	{
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary, T> LinkedDataPredicateObjects<I, V> for &T
where
	T: ?Sized + LinkedDataPredicateObjects<I, V>,
{
	fn accept_objects_visitor<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<I, V>,
	{
		T::accept_objects_visitor(self, visitor)
	}
}

impl<I: Interpretation, V: Vocabulary, T> LinkedDataPredicateObjects<I, V> for Box<T>
where
	T: ?Sized + LinkedDataPredicateObjects<I, V>,
{
	fn accept_objects_visitor<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<I, V>,
	{
		T::accept_objects_visitor(self, visitor)
	}
}

impl<I: Interpretation, V: Vocabulary, T> LinkedDataPredicateObjects<I, V> for Option<T>
where
	T: LinkedDataSubject<I, V> + LinkedDataResource<I, V>,
{
	fn accept_objects_visitor<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<I, V>,
	{
		if let Some(t) = self {
			visitor.visit_object(t)?;
		}

		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary, T> LinkedDataPredicateObjects<I, V> for [T]
where
	T: LinkedDataSubject<I, V> + LinkedDataResource<I, V>,
{
	fn accept_objects_visitor<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<I, V>,
	{
		for t in self {
			visitor.visit_object(t)?;
		}

		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary, T> LinkedDataPredicateObjects<I, V> for Vec<T>
where
	T: LinkedDataSubject<I, V> + LinkedDataResource<I, V>,
{
	fn accept_objects_visitor<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<I, V>,
	{
		for t in self {
			visitor.visit_object(t)?;
		}

		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary> LinkedDataPredicateObjects<I, V> for Iri
where
	V: IriVocabularyMut,
{
	fn accept_objects_visitor<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<I, V>,
	{
		visitor.visit_object(self)?;
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary> LinkedDataPredicateObjects<I, V> for IriBuf
where
	V: IriVocabularyMut,
{
	fn accept_objects_visitor<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<I, V>,
	{
		visitor.visit_object(self)?;
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary> LinkedDataPredicateObjects<I, V> for BlankId
where
	V: BlankIdVocabularyMut,
{
	fn accept_objects_visitor<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<I, V>,
	{
		visitor.visit_object(self)?;
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary> LinkedDataPredicateObjects<I, V> for BlankIdBuf
where
	V: BlankIdVocabularyMut,
{
	fn accept_objects_visitor<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<I, V>,
	{
		visitor.visit_object(self)?;
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary, T, B> LinkedDataPredicateObjects<I, V> for Id<T, B>
where
	T: LinkedDataPredicateObjects<I, V>,
	B: LinkedDataPredicateObjects<I, V>,
{
	fn accept_objects_visitor<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<I, V>,
	{
		match self {
			Self::Iri(i) => i.accept_objects_visitor(visitor),
			Self::Blank(b) => b.accept_objects_visitor(visitor),
		}
	}
}

pub trait PredicateObjectsVisitor<I: Interpretation, V: Vocabulary> {
	type Ok;
	type Error;

	fn visit_object<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataResource<I, V> + LinkedDataSubject<I, V>;

	fn end(self) -> Result<Self::Ok, Self::Error>;
}

pub trait LinkedDataDeserializePredicateObjects<I: Interpretation = (), V: Vocabulary = ()>:
	Sized
{
	fn deserialize_objects_in<'a, D>(
		vocabulary: &V,
		interpretation: &I,
		dataset: &D,
		graph: Option<&I::Resource>,
		objects: impl IntoIterator<Item = &'a I::Resource>,
		context: Context<I>,
	) -> Result<Self, FromLinkedDataError>
	where
		I::Resource: 'a,
		D: PatternMatchingDataset<Resource = I::Resource>;

	fn deserialize_objects<'a, D>(
		vocabulary: &V,
		interpretation: &I,
		dataset: &D,
		graph: Option<&I::Resource>,
		objects: impl IntoIterator<Item = &'a I::Resource>,
	) -> Result<Self, FromLinkedDataError>
	where
		I::Resource: 'a,
		D: PatternMatchingDataset<Resource = I::Resource>,
	{
		Self::deserialize_objects_in(
			vocabulary,
			interpretation,
			dataset,
			graph,
			objects,
			Context::default(),
		)
	}
}

macro_rules! deserialize_single_object {
	() => {
		fn deserialize_objects_in<'a, D>(
			vocabulary: &V,
			interpretation: &I,
			dataset: &D,
			graph: Option<&I::Resource>,
			objects: impl IntoIterator<Item = &'a I::Resource>,
			context: $crate::Context<I>,
		) -> Result<Self, FromLinkedDataError>
		where
			I::Resource: 'a,
			D: PatternMatchingDataset<Resource = I::Resource>,
		{
			use crate::LinkedDataDeserializeSubject;
			let mut objects = objects.into_iter();
			match objects.next() {
				Some(object) => {
					if objects.next().is_none() {
						Self::deserialize_subject_in(
							vocabulary,
							interpretation,
							dataset,
							graph,
							object,
							context,
						)
					} else {
						Err(FromLinkedDataError::TooManyValues(
							context.into_iris(vocabulary, interpretation),
						))
					}
				}
				None => Err(FromLinkedDataError::MissingRequiredValue(
					context.into_iris(vocabulary, interpretation),
				)),
			}
		}
	};
}

impl<I: Interpretation, V: Vocabulary> LinkedDataDeserializePredicateObjects<I, V> for IriBuf
where
	I: ReverseIriInterpretation<Iri = V::Iri>,
{
	deserialize_single_object!();
}

impl<I: Interpretation, V: Vocabulary> LinkedDataDeserializePredicateObjects<I, V> for BlankIdBuf
where
	I: ReverseIriInterpretation<Iri = V::Iri> + ReverseBlankIdInterpretation<BlankId = V::BlankId>,
{
	deserialize_single_object!();
}

impl<I: Interpretation, V: Vocabulary> LinkedDataDeserializePredicateObjects<I, V> for Id
where
	I: ReverseIdInterpretation<Iri = V::Iri, BlankId = V::BlankId>,
{
	deserialize_single_object!();
}

impl<I: Interpretation, V: Vocabulary, T> LinkedDataDeserializePredicateObjects<I, V> for Box<T>
where
	T: LinkedDataDeserializePredicateObjects<I, V>,
{
	fn deserialize_objects_in<'a, D>(
		vocabulary: &V,
		interpretation: &I,
		dataset: &D,
		graph: Option<&I::Resource>,
		objects: impl IntoIterator<Item = &'a I::Resource>,
		context: Context<I>,
	) -> Result<Self, FromLinkedDataError>
	where
		I::Resource: 'a,
		D: PatternMatchingDataset<Resource = I::Resource>,
	{
		T::deserialize_objects_in(vocabulary, interpretation, dataset, graph, objects, context)
			.map(Box::new)
	}
}

impl<I: Interpretation, V: Vocabulary, T> LinkedDataDeserializePredicateObjects<I, V> for Option<T>
where
	I: ReverseIriInterpretation<Iri = V::Iri>,
	T: LinkedDataDeserializeSubject<I, V>,
{
	fn deserialize_objects_in<'a, D>(
		vocabulary: &V,
		interpretation: &I,
		dataset: &D,
		graph: Option<&I::Resource>,
		objects: impl IntoIterator<Item = &'a I::Resource>,
		context: Context<I>,
	) -> Result<Self, FromLinkedDataError>
	where
		I::Resource: 'a,
		D: PatternMatchingDataset<Resource = I::Resource>,
	{
		let mut objects = objects.into_iter();
		match objects.next() {
			Some(object) => {
				if objects.next().is_none() {
					T::deserialize_subject_in(
						vocabulary,
						interpretation,
						dataset,
						graph,
						object,
						context,
					)
					.map(Some)
				} else {
					Err(FromLinkedDataError::TooManyValues(
						context.into_iris(vocabulary, interpretation),
					))
				}
			}
			None => Ok(None),
		}
	}
}
