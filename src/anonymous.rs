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

impl<'a, I: Interpretation, V: Vocabulary, T> LinkedDataResource<I, V> for AnonymousBinding<'a, T> {
	fn interpretation(
		&self,
		_vocabulary: &mut V,
		_interpretation: &mut I,
	) -> ResourceInterpretation<I, V> {
		ResourceInterpretation::Uninterpreted(None)
	}
}

impl<
		'a,
		V: Vocabulary + IriVocabularyMut,
		I: Interpretation,
		T: LinkedDataPredicateObjects<I, V>,
	> LinkedDataSubject<I, V> for AnonymousBinding<'a, T>
{
	fn visit_subject<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<I, V>,
	{
		serializer.predicate(self.0, self.1)?;
		serializer.end()
	}
}

impl<
		'a,
		V: Vocabulary + IriVocabularyMut,
		I: Interpretation,
		T: LinkedDataPredicateObjects<I, V>,
	> LinkedDataPredicateObjects<I, V> for AnonymousBinding<'a, T>
{
	fn visit_objects<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<I, V>,
	{
		serializer.object(self)?;
		serializer.end()
	}
}

impl<
		'a,
		V: Vocabulary + IriVocabularyMut,
		I: Interpretation,
		T: LinkedDataPredicateObjects<I, V>,
	> LinkedDataGraph<I, V> for AnonymousBinding<'a, T>
{
	fn visit_graph<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<I, V>,
	{
		serializer.subject(self)?;
		serializer.end()
	}
}

impl<
		'a,
		V: Vocabulary + IriVocabularyMut,
		I: Interpretation,
		T: LinkedDataPredicateObjects<I, V>,
	> LinkedData<I, V> for AnonymousBinding<'a, T>
{
	fn visit<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Visitor<I, V>,
	{
		serializer.default_graph(self)?;
		serializer.end()
	}
}
