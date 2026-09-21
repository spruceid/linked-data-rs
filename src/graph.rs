use iref::{Iri, IriBuf};
use rdf_types::{Interpretation, Vocabulary};

use crate::{LinkedData, LinkedDataPredicateObjects, LinkedDataResource, LinkedDataSubject};

// use crate::SerializeSubject;

/// Serialize a Linked-Data graph.
pub trait LinkedDataGraph<I: Interpretation, V: Vocabulary> {
	fn visit_graph<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<I, V>;
}

impl<I: Interpretation, V: Vocabulary> LinkedDataGraph<I, V> for () {
	fn visit_graph<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<I, V>,
	{
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary, T: ?Sized + LinkedDataGraph<I, V>> LinkedDataGraph<I, V>
	for &T
{
	fn visit_graph<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<I, V>,
	{
		T::visit_graph(self, visitor)
	}
}

impl<I: Interpretation, V: Vocabulary, T: ?Sized + LinkedDataGraph<I, V>> LinkedDataGraph<I, V>
	for Box<T>
{
	fn visit_graph<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<I, V>,
	{
		T::visit_graph(self, visitor)
	}
}

impl<I: Interpretation, V: Vocabulary> LinkedDataGraph<I, V> for Iri {
	fn visit_graph<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<I, V>,
	{
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary> LinkedDataGraph<I, V> for IriBuf {
	fn visit_graph<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<I, V>,
	{
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary, T: LinkedDataSubject<I, V> + LinkedDataResource<I, V>>
	LinkedDataGraph<I, V> for [T]
{
	fn visit_graph<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<I, V>,
	{
		for t in self {
			visitor.subject(t)?;
		}
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary, T: LinkedDataSubject<I, V> + LinkedDataResource<I, V>>
	LinkedDataGraph<I, V> for Vec<T>
{
	fn visit_graph<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<I, V>,
	{
		for t in self {
			visitor.subject(t)?;
		}
		visitor.end()
	}
}

pub trait GraphVisitor<I: Interpretation, V: Vocabulary> {
	type Ok;
	type Error;

	fn subject<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataResource<I, V> + LinkedDataSubject<I, V>;

	fn end(self) -> Result<Self::Ok, Self::Error>;
}

impl<I: Interpretation, V: Vocabulary, S: GraphVisitor<I, V>> GraphVisitor<I, V> for &mut S {
	type Ok = ();
	type Error = S::Error;

	fn subject<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataResource<I, V> + LinkedDataSubject<I, V>,
	{
		S::subject(self, value)
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(())
	}
}

pub struct AnonymousGraph<T>(pub T);

impl<I: Interpretation, V: Vocabulary, T> LinkedDataResource<I, V> for AnonymousGraph<T> {
	fn interpretation(
		&self,
		_vocabulary: &mut V,
		_interpretation: &mut I,
	) -> crate::ResourceInterpretation<'_, I, V> {
		crate::ResourceInterpretation::Uninterpreted(None)
	}
}

impl<I: Interpretation, V: Vocabulary, T: LinkedDataGraph<I, V>> LinkedDataSubject<I, V>
	for AnonymousGraph<T>
{
	fn visit_subject<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
	where
		S: crate::SubjectVisitor<I, V>,
	{
		serializer.graph(&self.0)?;
		serializer.end()
	}
}

impl<I: Interpretation, V: Vocabulary, T: LinkedDataGraph<I, V>> LinkedDataPredicateObjects<I, V>
	for AnonymousGraph<T>
{
	fn visit_objects<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: crate::PredicateObjectsVisitor<I, V>,
	{
		visitor.object(self)?;
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary, T: LinkedDataGraph<I, V>> LinkedDataGraph<I, V>
	for AnonymousGraph<T>
{
	fn visit_graph<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<I, V>,
	{
		T::visit_graph(&self.0, visitor)
	}
}

impl<I: Interpretation, V: Vocabulary, T: LinkedDataGraph<I, V>> LinkedData<I, V>
	for AnonymousGraph<T>
{
	fn visit<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: crate::Visitor<I, V>,
	{
		visitor.named_graph(self)?;
		visitor.end()
	}
}
