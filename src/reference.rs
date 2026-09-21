use rdf_types::{Interpretation, Vocabulary};

use crate::{
	LinkedData, LinkedDataGraph, LinkedDataPredicateObjects, LinkedDataResource, LinkedDataSubject,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
pub struct Ref<T>(pub T);

impl<T> From<T> for Ref<T> {
	fn from(value: T) -> Self {
		Self(value)
	}
}

impl<I: Interpretation, V: Vocabulary, T: LinkedDataResource<I, V>> LinkedDataResource<I, V>
	for Ref<T>
{
	fn interpretation(
		&self,
		vocabulary: &mut V,
		interpretation: &mut I,
	) -> crate::ResourceInterpretation<'_, I, V> {
		T::reference_interpretation(&self.0, vocabulary, interpretation)
	}
}

impl<I: Interpretation, V: Vocabulary, T> LinkedDataSubject<I, V> for Ref<T> {
	fn visit_subject<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: crate::SubjectVisitor<I, V>,
	{
		serializer.end()
	}
}

impl<I: Interpretation, V: Vocabulary, T: LinkedDataResource<I, V>> LinkedDataPredicateObjects<I, V>
	for Ref<T>
{
	fn visit_objects<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: crate::PredicateObjectsVisitor<I, V>,
	{
		visitor.object(self)?;
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary, T: LinkedDataResource<I, V>> LinkedDataGraph<I, V>
	for Ref<T>
{
	fn visit_graph<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: crate::GraphVisitor<I, V>,
	{
		visitor.subject(self)?;
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary, T: LinkedDataResource<I, V>> LinkedData<I, V> for Ref<T> {
	fn visit<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: crate::Visitor<I, V>,
	{
		visitor.default_graph(self)?;
		visitor.end()
	}
}
