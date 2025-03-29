use iref::{Iri, IriBuf};
use rdf_types::{
	dataset::PatternMatchingDataset,
	interpretation::{
		ReverseBlankIdInterpretation, ReverseIdInterpretation, ReverseIriInterpretation,
	},
	BlankId, BlankIdBuf, Id, Interpretation, Vocabulary,
};

use crate::{
	Context, FromLinkedDataError, LinkedDataGraph, LinkedDataPredicateObjects, LinkedDataResource,
};

pub trait LinkedDataSubject<I: Interpretation = (), V: Vocabulary = ()> {
	fn accept_subject_visitor<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<I, V>;
}

impl<I: Interpretation, V: Vocabulary> LinkedDataSubject<I, V> for () {
	fn accept_subject_visitor<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<I, V>,
	{
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary, T> LinkedDataSubject<I, V> for &T
where
	T: ?Sized + LinkedDataSubject<I, V>,
{
	fn accept_subject_visitor<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<I, V>,
	{
		T::accept_subject_visitor(self, visitor)
	}
}

impl<I: Interpretation, V: Vocabulary, T> LinkedDataSubject<I, V> for Box<T>
where
	T: ?Sized + LinkedDataSubject<I, V>,
{
	fn accept_subject_visitor<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<I, V>,
	{
		T::accept_subject_visitor(self, visitor)
	}
}

impl<I: Interpretation, V: Vocabulary> LinkedDataSubject<I, V> for Iri {
	fn accept_subject_visitor<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<I, V>,
	{
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary> LinkedDataSubject<I, V> for IriBuf {
	fn accept_subject_visitor<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<I, V>,
	{
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary> LinkedDataSubject<I, V> for BlankId {
	fn accept_subject_visitor<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<I, V>,
	{
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary> LinkedDataSubject<I, V> for BlankIdBuf {
	fn accept_subject_visitor<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<I, V>,
	{
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary, T, B> LinkedDataSubject<I, V> for Id<T, B>
where
	T: LinkedDataSubject<I, V>,
	B: LinkedDataSubject<I, V>,
{
	fn accept_subject_visitor<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<I, V>,
	{
		match self {
			Self::Iri(i) => i.accept_subject_visitor(visitor),
			Self::Blank(b) => b.accept_subject_visitor(visitor),
		}
	}
}

pub trait SubjectVisitor<I: Interpretation, V: Vocabulary> {
	type Ok;
	type Error;

	/// Visit a predicate of the graph.
	fn visit_predicate<L, T>(&mut self, predicate: &L, objects: &T) -> Result<(), Self::Error>
	where
		L: ?Sized + LinkedDataResource<I, V>,
		T: ?Sized + LinkedDataPredicateObjects<I, V>;

	/// Visit a reverse predicate of the graph.
	fn visit_reverse_predicate<L, T>(
		&mut self,
		predicate: &L,
		subjects: &T,
	) -> Result<(), Self::Error>
	where
		L: ?Sized + LinkedDataResource<I, V>,
		T: ?Sized + LinkedDataPredicateObjects<I, V>;

	fn visit_graph<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataGraph<I, V>;

	fn visit_include<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataResource<I, V> + LinkedDataSubject<I, V>;

	fn end(self) -> Result<Self::Ok, Self::Error>;
}

impl<I: Interpretation, V: Vocabulary, S> SubjectVisitor<I, V> for &mut S
where
	S: SubjectVisitor<I, V>,
{
	type Ok = ();
	type Error = S::Error;

	fn visit_predicate<L, T>(&mut self, predicate: &L, objects: &T) -> Result<(), Self::Error>
	where
		L: ?Sized + LinkedDataResource<I, V>,
		T: ?Sized + LinkedDataPredicateObjects<I, V>,
	{
		S::visit_predicate(self, predicate, objects)
	}

	fn visit_reverse_predicate<L, T>(
		&mut self,
		predicate: &L,
		subjects: &T,
	) -> Result<(), Self::Error>
	where
		L: ?Sized + LinkedDataResource<I, V>,
		T: ?Sized + LinkedDataPredicateObjects<I, V>,
	{
		S::visit_reverse_predicate(self, predicate, subjects)
	}

	fn visit_graph<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataGraph<I, V>,
	{
		S::visit_graph(self, value)
	}

	fn visit_include<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataResource<I, V> + LinkedDataSubject<I, V>,
	{
		S::visit_include(self, value)
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(())
	}
}

pub trait LinkedDataDeserializeSubject<I: Interpretation = (), V: Vocabulary = ()>: Sized {
	fn deserialize_subject_in<D>(
		vocabulary: &V,
		interpretation: &I,
		dataset: &D,
		graph: Option<&I::Resource>,
		resource: &I::Resource,
		context: Context<I>,
	) -> Result<Self, FromLinkedDataError>
	where
		D: PatternMatchingDataset<Resource = I::Resource>;

	fn deserialize_subject<D>(
		vocabulary: &V,
		interpretation: &I,
		dataset: &D,
		graph: Option<&I::Resource>,
		resource: &I::Resource,
	) -> Result<Self, FromLinkedDataError>
	where
		D: PatternMatchingDataset<Resource = I::Resource>,
	{
		Self::deserialize_subject_in(
			vocabulary,
			interpretation,
			dataset,
			graph,
			resource,
			Context::default(),
		)
	}
}

impl<I: Interpretation, V: Vocabulary> LinkedDataDeserializeSubject<I, V> for IriBuf
where
	I: ReverseIriInterpretation<Iri = V::Iri>,
{
	fn deserialize_subject_in<D>(
		vocabulary: &V,
		interpretation: &I,
		_dataset: &D,
		_graph: Option<&I::Resource>,
		resource: &I::Resource,
		context: Context<I>,
	) -> Result<Self, FromLinkedDataError>
	where
		D: PatternMatchingDataset<Resource = I::Resource>,
	{
		match interpretation.iris_of(resource).next() {
			Some(i) => {
				let iri = vocabulary.iri(i).unwrap();
				Ok(iri.to_owned())
			}
			None => Err(FromLinkedDataError::InvalidSubject {
				context: context.into_iris(vocabulary, interpretation),
				subject: None,
			}),
		}
	}
}

impl<I: Interpretation, V: Vocabulary> LinkedDataDeserializeSubject<I, V> for BlankIdBuf
where
	I: ReverseIriInterpretation<Iri = V::Iri> + ReverseBlankIdInterpretation<BlankId = V::BlankId>,
{
	fn deserialize_subject_in<D>(
		vocabulary: &V,
		interpretation: &I,
		_dataset: &D,
		_graph: Option<&I::Resource>,
		resource: &I::Resource,
		context: Context<I>,
	) -> Result<Self, FromLinkedDataError>
	where
		D: PatternMatchingDataset<Resource = I::Resource>,
	{
		match interpretation.blank_ids_of(resource).next() {
			Some(b) => {
				let blank_id = vocabulary.blank_id(b).unwrap();
				Ok(blank_id.to_owned())
			}
			None => Err(FromLinkedDataError::InvalidSubject {
				context: context.into_iris(vocabulary, interpretation),
				subject: interpretation
					.iris_of(resource)
					.next()
					.map(|i| vocabulary.iri(i).unwrap().to_owned()),
			}),
		}
	}
}

impl<I: Interpretation, V: Vocabulary> LinkedDataDeserializeSubject<I, V> for Id
where
	I: ReverseIdInterpretation<Iri = V::Iri, BlankId = V::BlankId>,
{
	fn deserialize_subject_in<D>(
		vocabulary: &V,
		interpretation: &I,
		_dataset: &D,
		_graph: Option<&I::Resource>,
		resource: &I::Resource,
		context: Context<I>,
	) -> Result<Self, FromLinkedDataError>
	where
		D: PatternMatchingDataset<Resource = I::Resource>,
	{
		match interpretation.ids_of(resource).next() {
			Some(Id::Iri(i)) => {
				let iri = vocabulary.iri(i).unwrap();
				Ok(Id::Iri(iri.to_owned()))
			}
			Some(Id::Blank(b)) => {
				let blank_id = vocabulary.blank_id(b).unwrap();
				Ok(Id::Blank(blank_id.to_owned()))
			}
			None => Err(FromLinkedDataError::InvalidSubject {
				context: context.into_iris(vocabulary, interpretation),
				subject: None,
			}),
		}
	}
}

impl<I: Interpretation, V: Vocabulary, T: LinkedDataDeserializeSubject<I, V>>
	LinkedDataDeserializeSubject<I, V> for Box<T>
{
	fn deserialize_subject_in<D>(
		vocabulary: &V,
		interpretation: &I,
		dataset: &D,
		graph: Option<&I::Resource>,
		resource: &I::Resource,
		context: Context<I>,
	) -> Result<Self, FromLinkedDataError>
	where
		D: PatternMatchingDataset<Resource = I::Resource>,
	{
		T::deserialize_subject_in(
			vocabulary,
			interpretation,
			dataset,
			graph,
			resource,
			context,
		)
		.map(Box::new)
	}
}
