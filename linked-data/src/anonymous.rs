use iref::Iri;
use rdf_types::{Interpretation, IriVocabularyMut, Vocabulary};

use crate::{
	GraphVisitor, Interpret, LinkedData, LinkedDataGraph, LinkedDataPredicateObjects,
	LinkedDataSubject, PredicateObjectsVisitor, ResourceInterpretation, SubjectVisitor, Visitor,
};

pub struct AnonymousBinding<'a, T>(pub &'a Iri, pub &'a T);

impl<'a, T> AnonymousBinding<'a, T> {
	pub fn new(iri: &'a Iri, value: &'a T) -> Self {
		Self(iri, value)
	}
}

impl<'a, V: Vocabulary, I: Interpretation, T> Interpret<V, I> for AnonymousBinding<'a, T> {
	fn interpret(
		&self,
		_interpretation: &mut I,
		_vocabulary: &mut V,
	) -> ResourceInterpretation<V, I> {
		ResourceInterpretation::Uninterpreted(None)
	}
}

impl<
		'a,
		V: Vocabulary + IriVocabularyMut,
		I: Interpretation,
		T: LinkedDataPredicateObjects<V, I>,
	> LinkedDataSubject<V, I> for AnonymousBinding<'a, T>
{
	fn visit_subject<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<V, I>,
	{
		serializer.predicate(self.0, self.1)?;
		serializer.end()
	}
}

impl<
		'a,
		V: Vocabulary + IriVocabularyMut,
		I: Interpretation,
		T: LinkedDataPredicateObjects<V, I>,
	> LinkedDataPredicateObjects<V, I> for AnonymousBinding<'a, T>
{
	fn visit_objects<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<V, I>,
	{
		serializer.object(self)?;
		serializer.end()
	}
}

impl<
		'a,
		V: Vocabulary + IriVocabularyMut,
		I: Interpretation,
		T: LinkedDataPredicateObjects<V, I>,
	> LinkedDataGraph<V, I> for AnonymousBinding<'a, T>
{
	fn visit_graph<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<V, I>,
	{
		serializer.subject(self)?;
		serializer.end()
	}
}

impl<
		'a,
		V: Vocabulary + IriVocabularyMut,
		I: Interpretation,
		T: LinkedDataPredicateObjects<V, I>,
	> LinkedData<V, I> for AnonymousBinding<'a, T>
{
	fn visit<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Visitor<V, I>,
	{
		serializer.default_graph(self)?;
		serializer.end()
	}
}
