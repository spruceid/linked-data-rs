use iref::{Iri, IriBuf};
use rdf_types::{Interpretation, Vocabulary};

use crate::{Interpret, LinkedDataSubject};

// use crate::SerializeSubject;

/// Serialize a Linked-Data graph.
pub trait LinkedDataGraph<V: Vocabulary, I: Interpretation> {
	fn visit_graph<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<V, I>;
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
		T: ?Sized + Interpret<V, I> + LinkedDataSubject<V, I>;

	fn end(self) -> Result<Self::Ok, Self::Error>;
}

impl<'a, V: Vocabulary, I: Interpretation, S: GraphVisitor<V, I>> GraphVisitor<V, I> for &'a mut S {
	type Ok = ();
	type Error = S::Error;

	fn subject<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + Interpret<V, I> + LinkedDataSubject<V, I>,
	{
		S::subject(self, value)
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(())
	}
}
