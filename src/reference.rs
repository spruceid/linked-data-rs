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

impl<I: Interpretation, V: Vocabulary, T> LinkedDataResource<I, V> for Ref<T>
where
	T: LinkedDataResource<I, V>,
{
	fn interpretation(
		&self,
		vocabulary: &mut V,
		interpretation: &mut I,
	) -> crate::ResourceInterpretation<I, V> {
		T::reference_interpretation(&self.0, vocabulary, interpretation)
	}
}

impl<I: Interpretation, V: Vocabulary, T> LinkedDataSubject<I, V> for Ref<T> {
	fn accept_subject_visitor<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: crate::SubjectVisitor<I, V>,
	{
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary, T> LinkedDataPredicateObjects<I, V> for Ref<T>
where
	T: LinkedDataResource<I, V>,
{
	fn accept_objects_visitor<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: crate::PredicateObjectsVisitor<I, V>,
	{
		visitor.visit_object(self)?;
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary, T> LinkedDataGraph<I, V> for Ref<T>
where
	T: LinkedDataResource<I, V>,
{
	fn accept_graph_visitor<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: crate::GraphVisitor<I, V>,
	{
		visitor.visit_subject(self)?;
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary, T> LinkedData<I, V> for Ref<T>
where
	T: LinkedDataResource<I, V>,
{
	fn accept_visitor<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: crate::Visitor<I, V>,
	{
		visitor.visit_default_graph(self)?;
		visitor.end()
	}
}
