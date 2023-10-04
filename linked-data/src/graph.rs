use iref::{Iri, IriBuf};
use rdf_types::{Interpretation, Vocabulary};

use crate::{LinkedData, LinkedDataPredicateObjects, LinkedDataResource, LinkedDataSubject};

// use crate::SerializeSubject;

/// Serialize a Linked-Data graph.
pub trait LinkedDataGraph<V: Vocabulary, I: Interpretation> {
	fn visit_graph<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<V, I>;
}

impl<V: Vocabulary, I: Interpretation> LinkedDataGraph<V, I> for () {
	fn visit_graph<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<V, I>,
	{
		visitor.end()
	}
}

impl<'a, V: Vocabulary, I: Interpretation, T: ?Sized + LinkedDataGraph<V, I>> LinkedDataGraph<V, I>
	for &'a T
{
	fn visit_graph<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<V, I>,
	{
		T::visit_graph(self, visitor)
	}
}

impl<V: Vocabulary, I: Interpretation, T: ?Sized + LinkedDataGraph<V, I>> LinkedDataGraph<V, I>
	for Box<T>
{
	fn visit_graph<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<V, I>,
	{
		T::visit_graph(self, visitor)
	}
}

impl<V: Vocabulary, I: Interpretation> LinkedDataGraph<V, I> for Iri {
	fn visit_graph<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<V, I>,
	{
		visitor.end()
	}
}

impl<V: Vocabulary, I: Interpretation> LinkedDataGraph<V, I> for IriBuf {
	fn visit_graph<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<V, I>,
	{
		visitor.end()
	}
}

pub trait GraphVisitor<V: Vocabulary, I: Interpretation> {
	type Ok;
	type Error;

	fn subject<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataResource<V, I> + LinkedDataSubject<V, I>;

	fn end(self) -> Result<Self::Ok, Self::Error>;
}

impl<'a, V: Vocabulary, I: Interpretation, S: GraphVisitor<V, I>> GraphVisitor<V, I> for &'a mut S {
	type Ok = ();
	type Error = S::Error;

	fn subject<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataResource<V, I> + LinkedDataSubject<V, I>,
	{
		S::subject(self, value)
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(())
	}
}

pub struct AnonymousGraph<T>(pub T);

impl<V: Vocabulary, I: Interpretation, T> LinkedDataResource<V, I> for AnonymousGraph<T> {
	fn interpretation(
		&self,
		_vocabulary: &mut V,
		_interpretation: &mut I,
	) -> crate::ResourceInterpretation<V, I> {
		crate::ResourceInterpretation::Uninterpreted(None)
	}
}

impl<V: Vocabulary, I: Interpretation, T: LinkedDataGraph<V, I>> LinkedDataSubject<V, I>
	for AnonymousGraph<T>
{
	fn visit_subject<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
	where
		S: crate::SubjectVisitor<V, I>,
	{
		serializer.graph(&self.0)?;
		serializer.end()
	}
}

impl<V: Vocabulary, I: Interpretation, T: LinkedDataGraph<V, I>> LinkedDataPredicateObjects<V, I>
	for AnonymousGraph<T>
{
	fn visit_objects<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: crate::PredicateObjectsVisitor<V, I>,
	{
		visitor.object(self)?;
		visitor.end()
	}
}

impl<V: Vocabulary, I: Interpretation, T: LinkedDataGraph<V, I>> LinkedDataGraph<V, I>
	for AnonymousGraph<T>
{
	fn visit_graph<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<V, I>,
	{
		T::visit_graph(&self.0, visitor)
	}
}

impl<V: Vocabulary, I: Interpretation, T: LinkedDataGraph<V, I>> LinkedData<V, I>
	for AnonymousGraph<T>
{
	fn visit<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: crate::Visitor<V, I>,
	{
		visitor.named_graph(self)?;
		visitor.end()
	}
}
