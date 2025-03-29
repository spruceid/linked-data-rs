use iref::Iri;
use rdf_types::{vocabulary::IriVocabularyMut, Interpretation, Vocabulary};

use crate::{
	GraphVisitor, LinkedData, LinkedDataGraph, LinkedDataPredicateObjects, LinkedDataResource,
	LinkedDataSubject, PredicateObjectsVisitor, ResourceInterpretation, SubjectVisitor, Visitor,
};

pub struct AnonymousBinding<'a, T>(pub &'a Iri, pub &'a T);

impl<'a, T> AnonymousBinding<'a, T> {
	pub fn new(iri: &'a Iri, value: &'a T) -> Self {
		Self(iri, value)
	}
}

impl<I: Interpretation, V: Vocabulary, T> LinkedDataResource<I, V> for AnonymousBinding<'_, T> {
	fn interpretation(
		&self,
		_vocabulary: &mut V,
		_interpretation: &mut I,
	) -> ResourceInterpretation<I, V> {
		ResourceInterpretation::Uninterpreted(None)
	}
}

impl<I: Interpretation, V: Vocabulary, T> LinkedDataSubject<I, V> for AnonymousBinding<'_, T>
where
	V: IriVocabularyMut,
	T: LinkedDataPredicateObjects<I, V>,
{
	fn accept_subject_visitor<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<I, V>,
	{
		visitor.visit_predicate(self.0, self.1)?;
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary, T> LinkedDataPredicateObjects<I, V>
	for AnonymousBinding<'_, T>
where
	V: IriVocabularyMut,
	T: LinkedDataPredicateObjects<I, V>,
{
	fn accept_objects_visitor<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<I, V>,
	{
		visitor.visit_object(self)?;
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary, T> LinkedDataGraph<I, V> for AnonymousBinding<'_, T>
where
	V: IriVocabularyMut,
	T: LinkedDataPredicateObjects<I, V>,
{
	fn accept_graph_visitor<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<I, V>,
	{
		visitor.visit_subject(self)?;
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary, T> LinkedData<I, V> for AnonymousBinding<'_, T>
where
	V: IriVocabularyMut,
	T: LinkedDataPredicateObjects<I, V>,
{
	fn accept_visitor<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: Visitor<I, V>,
	{
		visitor.visit_default_graph(self)?;
		visitor.end()
	}
}
