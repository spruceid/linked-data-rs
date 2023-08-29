use rdf_types::{Interpretation, Vocabulary};

use crate::{Interpret, LinkedDataSubject};

// use crate::SerializeSubject;

/// Serialize a Linked-Data graph.
pub trait LinkedDataGraph<V: Vocabulary, I: Interpretation> {
	fn visit_graph<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<V, I>;
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
