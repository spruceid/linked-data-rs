use iref::{Iri, IriBuf};
use rdf_types::{Interpretation, Vocabulary};

use crate::{LinkedData, LinkedDataPredicateObjects, LinkedDataResource, LinkedDataSubject};

// use crate::SerializeSubject;

pub trait LinkedDataGraph<I: Interpretation, V: Vocabulary> {
	fn accept_graph_visitor<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<I, V>;
}

impl<I: Interpretation, V: Vocabulary> LinkedDataGraph<I, V> for () {
	fn accept_graph_visitor<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<I, V>,
	{
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary, T> LinkedDataGraph<I, V> for &T
where
	T: ?Sized + LinkedDataGraph<I, V>,
{
	fn accept_graph_visitor<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<I, V>,
	{
		T::accept_graph_visitor(self, visitor)
	}
}

impl<I: Interpretation, V: Vocabulary, T> LinkedDataGraph<I, V> for Box<T>
where
	T: ?Sized + LinkedDataGraph<I, V>,
{
	fn accept_graph_visitor<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<I, V>,
	{
		T::accept_graph_visitor(self, visitor)
	}
}

impl<I: Interpretation, V: Vocabulary> LinkedDataGraph<I, V> for Iri {
	fn accept_graph_visitor<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<I, V>,
	{
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary> LinkedDataGraph<I, V> for IriBuf {
	fn accept_graph_visitor<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<I, V>,
	{
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary, T> LinkedDataGraph<I, V> for [T]
where
	T: LinkedDataSubject<I, V> + LinkedDataResource<I, V>,
{
	fn accept_graph_visitor<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<I, V>,
	{
		for t in self {
			visitor.visit_subject(t)?;
		}
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary, T> LinkedDataGraph<I, V> for Vec<T>
where
	T: LinkedDataSubject<I, V> + LinkedDataResource<I, V>,
{
	fn accept_graph_visitor<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<I, V>,
	{
		for t in self {
			visitor.visit_subject(t)?;
		}
		visitor.end()
	}
}

pub trait GraphVisitor<I: Interpretation, V: Vocabulary> {
	type Ok;
	type Error;

	fn visit_subject<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataResource<I, V> + LinkedDataSubject<I, V>;

	fn end(self) -> Result<Self::Ok, Self::Error>;
}

impl<I: Interpretation, V: Vocabulary, S> GraphVisitor<I, V> for &mut S
where
	S: GraphVisitor<I, V>,
{
	type Ok = ();
	type Error = S::Error;

	fn visit_subject<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataResource<I, V> + LinkedDataSubject<I, V>,
	{
		S::visit_subject(self, value)
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
	) -> crate::ResourceInterpretation<I, V> {
		crate::ResourceInterpretation::Uninterpreted(None)
	}
}

impl<I: Interpretation, V: Vocabulary, T> LinkedDataSubject<I, V> for AnonymousGraph<T>
where
	T: LinkedDataGraph<I, V>,
{
	fn accept_subject_visitor<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: crate::SubjectVisitor<I, V>,
	{
		visitor.visit_graph(&self.0)?;
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary, T> LinkedDataPredicateObjects<I, V> for AnonymousGraph<T>
where
	T: LinkedDataGraph<I, V>,
{
	fn accept_objects_visitor<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: crate::PredicateObjectsVisitor<I, V>,
	{
		visitor.visit_object(self)?;
		visitor.end()
	}
}

impl<I: Interpretation, V: Vocabulary, T> LinkedDataGraph<I, V> for AnonymousGraph<T>
where
	T: LinkedDataGraph<I, V>,
{
	fn accept_graph_visitor<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<I, V>,
	{
		T::accept_graph_visitor(&self.0, visitor)
	}
}

impl<I: Interpretation, V: Vocabulary, T> LinkedData<I, V> for AnonymousGraph<T>
where
	T: LinkedDataGraph<I, V>,
{
	fn accept_visitor<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: crate::Visitor<I, V>,
	{
		visitor.visit_named_graph(self)?;
		visitor.end()
	}
}
