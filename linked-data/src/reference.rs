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

impl<V: Vocabulary, I: Interpretation, T: LinkedDataResource<V, I>> LinkedDataResource<V, I>
	for Ref<T>
{
	fn interpretation(
		&self,
		vocabulary: &mut V,
		interpretation: &mut I,
	) -> crate::ResourceInterpretation<V, I> {
		T::reference_interpretation(&self.0, vocabulary, interpretation)
	}
}

impl<V: Vocabulary, I: Interpretation, T> LinkedDataSubject<V, I> for Ref<T> {
	fn visit_subject<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: crate::SubjectVisitor<V, I>,
	{
		serializer.end()
	}
}

impl<V: Vocabulary, I: Interpretation, T: LinkedDataResource<V, I>> LinkedDataPredicateObjects<V, I>
	for Ref<T>
{
	fn visit_objects<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: crate::PredicateObjectsVisitor<V, I>,
	{
		visitor.object(self)?;
		visitor.end()
	}
}

impl<V: Vocabulary, I: Interpretation, T: LinkedDataResource<V, I>> LinkedDataGraph<V, I>
	for Ref<T>
{
	fn visit_graph<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: crate::GraphVisitor<V, I>,
	{
		visitor.subject(self)?;
		visitor.end()
	}
}

impl<V: Vocabulary, I: Interpretation, T: LinkedDataResource<V, I>> LinkedData<V, I> for Ref<T> {
	fn visit<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: crate::Visitor<V, I>,
	{
		visitor.default_graph(self)?;
		visitor.end()
	}
}
