use rdf_types::{Interpretation, Vocabulary};

use crate::{Interpret, LinkedDataSubject};

/// Type representing the objects of an RDF subject's predicate binding.
pub trait LinkedDataPredicateObjects<V: Vocabulary, I: Interpretation> {
	fn visit_objects<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<V, I>;
}

impl<'a, V: Vocabulary, I: Interpretation, T: LinkedDataPredicateObjects<V, I>>
	LinkedDataPredicateObjects<V, I> for &'a T
{
	fn visit_objects<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<V, I>,
	{
		T::visit_objects(self, visitor)
	}
}

pub trait PredicateObjectsVisitor<V: Vocabulary, I: Interpretation> {
	type Ok;
	type Error;

	fn object<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + Interpret<V, I> + LinkedDataSubject<V, I>;

	fn end(self) -> Result<Self::Ok, Self::Error>;
}
