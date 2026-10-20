use std::fmt::{Debug, Display};

use rdf_syntax::{
	CowGroundTerm, CowTerm, Iri, IriBuf, Quad, RDF_FIRST, RDF_NIL, RDF_REST, Term,
	pattern::LinearQuadPattern,
};

mod dataset;
mod flatten;
mod quads;
mod replay;
mod resources;

pub use dataset::*;
pub use flatten::*;
pub use quads::*;
pub use replay::*;
pub use resources::*;

pub trait DeserializeLinkedDataWith<Q, R: Clone + Ord = Term>: Sized {
	fn deserialize_rdf_with<D>(
		state: Q,
		deserializer: D,
		graph: Option<&R>,
	) -> Result<Self, D::Error>
	where
		D: LinkedDataDeserializer<R>;

	fn deserialize_rdf_objects_with<D>(
		state: Q,
		mut deserializer: D,
		subject: &R,
		predicate: &R,
		graph: Option<&R>,
	) -> Result<Self, D::Error>
	where
		D: LinkedDataDeserializer<R>,
	{
		let mut objects = Vec::new();

		while let Some(quad) = deserializer.deserialize_quad(Quad(
			Some(subject),
			Some(predicate),
			None,
			Some(graph),
		))? {
			objects.push(quad.into_object())
		}

		Self::deserialize_rdf_with(
			state,
			ResourcesDeserializer::new(deserializer, objects),
			graph,
		)
	}
}

pub trait DeserializeLinkedData<R: Clone + Ord = Term>: Sized {
	fn deserialize_rdf<D>(deserializer: D, graph: Option<&R>) -> Result<Self, D::Error>
	where
		D: LinkedDataDeserializer<R>;

	fn deserialize_rdf_objects<D>(
		mut deserializer: D,
		subject: &R,
		predicate: &R,
		graph: Option<&R>,
	) -> Result<Self, D::Error>
	where
		D: LinkedDataDeserializer<R>,
	{
		let mut objects = Vec::new();

		while let Some(quad) = deserializer.deserialize_quad(Quad(
			Some(subject),
			Some(predicate),
			None,
			Some(graph),
		))? {
			objects.push(quad.into_object())
		}

		Self::deserialize_rdf(ResourcesDeserializer::new(deserializer, objects), graph)
	}
}

impl<Q, R: Clone + Ord, T: DeserializeLinkedDataWith<Q, R>> DeserializeLinkedDataWith<Q, R>
	for Option<T>
{
	fn deserialize_rdf_with<D>(
		state: Q,
		deserializer: D,
		graph: Option<&R>,
	) -> Result<Self, D::Error>
	where
		D: LinkedDataDeserializer<R>,
	{
		if deserializer.peek_resources()?.next().transpose()?.is_some() {
			T::deserialize_rdf_with(state, deserializer, graph).map(Some)
		} else {
			Ok(None)
		}
	}
}

impl<R: Clone + Ord, T: DeserializeLinkedData<R>> DeserializeLinkedData<R> for Option<T> {
	fn deserialize_rdf<D>(deserializer: D, graph: Option<&R>) -> Result<Self, D::Error>
	where
		D: LinkedDataDeserializer<R>,
	{
		if deserializer.peek_resources()?.next().transpose()?.is_some() {
			T::deserialize_rdf(deserializer, graph).map(Some)
		} else {
			Ok(None)
		}
	}
}

impl<R: Clone + Ord> DeserializeLinkedData<R> for Term {
	fn deserialize_rdf<D>(mut deserializer: D, _graph: Option<&R>) -> Result<Self, D::Error>
	where
		D: LinkedDataDeserializer<R>,
	{
		let this = deserializer
			.deserialize_resource(None)?
			.ok_or_else(|| Error::missing_resource())?;

		if let Some(term) = deserializer.terms_of(&this)?.next() {
			return Ok(term?.into_owned());
		}

		Err(Error::custom("no IRI representation"))
	}
}

impl<R: Clone + Ord> DeserializeLinkedData<R> for IriBuf {
	fn deserialize_rdf<D>(mut deserializer: D, _graph: Option<&R>) -> Result<Self, D::Error>
	where
		D: LinkedDataDeserializer<R>,
	{
		let this = deserializer
			.deserialize_resource(None)?
			.ok_or_else(|| Error::missing_resource())?;

		for term in deserializer.terms_of(&this)? {
			if let CowTerm::Ground(CowGroundTerm::Iri(iri)) = term? {
				return Ok(iri.into_owned());
			}
		}

		Err(Error::custom("no IRI representation"))
	}
}

impl<Q, T, R: Clone + Ord> DeserializeLinkedDataWith<Q, R> for Vec<T>
where
	Q: Clone,
	T: DeserializeLinkedDataWith<Q, R>,
{
	fn deserialize_rdf_with<D>(
		state: Q,
		mut deserializer: D,
		graph: Option<&R>,
	) -> Result<Self, D::Error>
	where
		D: LinkedDataDeserializer<R>,
	{
		let mut result = Vec::new();

		let mut head = deserializer
			.deserialize_resource(None)?
			.ok_or_else(Error::missing_resource)?;

		let prop_first = deserializer.interpret(RDF_FIRST.into())?;
		let prop_rest = deserializer.interpret(RDF_REST.into())?;

		'list: loop {
			for term in deserializer.terms_of(&head)? {
				if term? == RDF_NIL {
					break 'list;
				}
			}

			result.push(T::deserialize_rdf_objects_with(
				state.clone(),
				deserializer.as_dyn_mut(),
				&head,
				&prop_first,
				graph,
			)?);

			head = deserializer
				.deserialize_quad(Quad(Some(&head), Some(&prop_rest), None, Some(graph)))?
				.ok_or_else(Error::missing_quad)?
				.into_object();
		}

		Ok(result)
	}
}

impl<T, R: Clone + Ord> DeserializeLinkedData<R> for Vec<T>
where
	T: DeserializeLinkedData<R>,
{
	fn deserialize_rdf<D>(mut deserializer: D, graph: Option<&R>) -> Result<Self, D::Error>
	where
		D: LinkedDataDeserializer<R>,
	{
		let mut result = Vec::new();

		let mut head = deserializer
			.deserialize_resource(None)?
			.ok_or_else(Error::missing_resource)?;

		let prop_first = deserializer.interpret(RDF_FIRST.into())?;
		let prop_rest = deserializer.interpret(RDF_REST.into())?;

		'list: loop {
			for term in deserializer.terms_of(&head)? {
				if term? == RDF_NIL {
					break 'list;
				}
			}

			result.push(T::deserialize_rdf_objects(
				deserializer.as_dyn_mut(),
				&head,
				&prop_first,
				graph,
			)?);

			head = deserializer
				.deserialize_quad(Quad(Some(&head), Some(&prop_rest), None, Some(graph)))?
				.ok_or_else(Error::missing_quad)?
				.into_object();
		}

		Ok(result)
	}
}

pub trait LinkedDataDeserializer<R = Term> {
	type Error: Error<R>;

	fn as_dyn_mut<'a>(
		&'a mut self,
	) -> &'a mut (dyn 'a + DynLinkedDataDeserializer<R, Error = Self::Error>)
	where
		Self: Sized,
	{
		self
	}

	fn interpret(&mut self, term: CowTerm) -> Result<R, Self::Error>;

	fn terms_of<'a>(
		&'a self,
		resource: &'a R,
	) -> Result<impl Iterator<Item = Result<CowTerm<'a>, Self::Error>>, Self::Error>;

	fn is_iri(&self, resource: &R, iri: &Iri) -> Result<bool, Self::Error> {
		for term in self.terms_of(resource)? {
			if term? == iri {
				return Ok(true);
			}
		}

		Ok(false)
	}

	fn peek_resources<'a>(
		&'a self,
	) -> Result<impl Iterator<Item = Result<&'a R, Self::Error>>, Self::Error>
	where
		R: 'a;

	fn peek_resource<'a>(&'a self) -> Result<Option<&'a R>, Self::Error>
	where
		R: 'a,
	{
		self.peek_resources()?.next().transpose()
	}

	fn deserialize_resource(&mut self, resource: Option<R>) -> Result<Option<R>, Self::Error>;

	fn peek_quads<'p>(
		&'p self,
		pattern: impl Into<LinearQuadPattern<&'p R>>,
	) -> Result<impl Iterator<Item = Result<Quad<&'p R>, Self::Error>>, Self::Error>
	where
		R: 'p;

	fn peek_quad<'p>(
		&'p self,
		pattern: impl Into<LinearQuadPattern<&'p R>>,
	) -> Result<Option<Quad<&'p R>>, Self::Error>
	where
		R: 'p,
	{
		self.peek_quads(pattern)?.next().transpose()
	}

	fn deserialize_quad<'p>(
		&mut self,
		pattern: impl Into<LinearQuadPattern<&'p R>>,
	) -> Result<Option<Quad<R>>, Self::Error>
	where
		R: 'p;
}

/// Dyn-compatible version of `LinkedDataDeserializer`.
pub trait DynLinkedDataDeserializer<R = Term> {
	type Error: Error<R>;

	fn interpret(&mut self, term: CowTerm) -> Result<R, Self::Error>;

	fn terms_of<'a>(
		&'a self,
		resource: &'a R,
	) -> Result<DynTermIterator<'a, Self::Error>, Self::Error>;

	fn peek_resources<'a>(&'a self) -> Result<DynResourceIterator<'a, R, Self::Error>, Self::Error>
	where
		R: 'a;

	fn deserialize_resource(&mut self, resource: Option<R>) -> Result<Option<R>, Self::Error>;

	fn peek_quads<'p>(
		&'p self,
		pattern: LinearQuadPattern<&'p R>,
	) -> Result<DynQuadIterator<'p, R, Self::Error>, Self::Error>
	where
		R: 'p;

	fn deserialize_quad<'p>(
		&mut self,
		pattern: LinearQuadPattern<&'p R>,
	) -> Result<Option<Quad<R>>, Self::Error>
	where
		R: 'p;
}

/// Boxed type-erased term iterator.
pub type DynTermIterator<'a, E> = Box<dyn 'a + Iterator<Item = Result<CowTerm<'a>, E>>>;

/// Boxed type-erased resource iterator.
pub type DynResourceIterator<'a, R, E> = Box<dyn 'a + Iterator<Item = Result<&'a R, E>>>;

/// Boxed type-erased quad iterator.
pub type DynQuadIterator<'a, R, E> = Box<dyn 'a + Iterator<Item = Result<Quad<&'a R>, E>>>;

impl<'d, R, E> LinkedDataDeserializer<R> for dyn 'd + DynLinkedDataDeserializer<R, Error = E>
where
	E: Error<R>,
{
	type Error = E;

	fn interpret(&mut self, term: CowTerm) -> Result<R, Self::Error> {
		DynLinkedDataDeserializer::interpret(self, term)
	}

	fn terms_of<'a>(
		&'a self,
		resource: &'a R,
	) -> Result<impl Iterator<Item = Result<CowTerm<'a>, Self::Error>>, Self::Error> {
		DynLinkedDataDeserializer::terms_of(self, resource)
	}

	fn peek_resources<'a>(
		&'a self,
	) -> Result<impl Iterator<Item = Result<&'a R, Self::Error>>, Self::Error>
	where
		R: 'a,
	{
		DynLinkedDataDeserializer::peek_resources(self)
	}

	fn deserialize_resource(&mut self, resource: Option<R>) -> Result<Option<R>, Self::Error> {
		DynLinkedDataDeserializer::deserialize_resource(self, resource)
	}

	fn peek_quads<'p>(
		&'p self,
		pattern: impl Into<LinearQuadPattern<&'p R>>,
	) -> Result<impl Iterator<Item = Result<Quad<&'p R>, Self::Error>>, Self::Error>
	where
		R: 'p,
	{
		DynLinkedDataDeserializer::peek_quads(self, pattern.into())
	}

	fn deserialize_quad<'p>(
		&mut self,
		pattern: impl Into<LinearQuadPattern<&'p R>>,
	) -> Result<Option<Quad<R>>, Self::Error>
	where
		R: 'p,
	{
		DynLinkedDataDeserializer::deserialize_quad(self, pattern.into())
	}
}

impl<R, D> DynLinkedDataDeserializer<R> for D
where
	D: LinkedDataDeserializer<R>,
{
	type Error = D::Error;

	fn interpret(&mut self, term: CowTerm) -> Result<R, Self::Error> {
		LinkedDataDeserializer::interpret(self, term)
	}

	fn terms_of<'a>(
		&'a self,
		resource: &'a R,
	) -> Result<Box<dyn 'a + Iterator<Item = Result<CowTerm<'a>, Self::Error>>>, Self::Error> {
		LinkedDataDeserializer::terms_of(self, resource).map(|i| Box::new(i) as _)
	}

	fn peek_resources<'a>(
		&'a self,
	) -> Result<Box<dyn 'a + Iterator<Item = Result<&'a R, Self::Error>>>, Self::Error>
	where
		R: 'a,
	{
		LinkedDataDeserializer::peek_resources(self).map(|i| Box::new(i) as _)
	}

	fn deserialize_resource(&mut self, resource: Option<R>) -> Result<Option<R>, Self::Error> {
		LinkedDataDeserializer::deserialize_resource(self, resource)
	}

	fn peek_quads<'p>(
		&'p self,
		pattern: LinearQuadPattern<&'p R>,
	) -> Result<Box<dyn 'p + Iterator<Item = Result<Quad<&'p R>, Self::Error>>>, Self::Error>
	where
		R: 'p,
	{
		LinkedDataDeserializer::peek_quads(self, pattern).map(|i| Box::new(i) as _)
	}

	fn deserialize_quad<'p>(
		&mut self,
		pattern: LinearQuadPattern<&'p R>,
	) -> Result<Option<Quad<R>>, Self::Error>
	where
		R: 'p,
	{
		LinkedDataDeserializer::deserialize_quad(self, pattern)
	}
}

impl<R, S: ?Sized + LinkedDataDeserializer<R>> LinkedDataDeserializer<R> for &mut S {
	type Error = S::Error;

	fn interpret(&mut self, term: CowTerm) -> Result<R, Self::Error> {
		S::interpret(*self, term)
	}

	fn terms_of<'a>(
		&'a self,
		resource: &'a R,
	) -> Result<impl Iterator<Item = Result<CowTerm<'a>, Self::Error>>, Self::Error> {
		S::terms_of(*self, resource)
	}

	fn peek_resources<'a>(
		&'a self,
	) -> Result<impl Iterator<Item = Result<&'a R, Self::Error>>, Self::Error>
	where
		R: 'a,
	{
		S::peek_resources(*self)
	}

	fn deserialize_resource(&mut self, resource: Option<R>) -> Result<Option<R>, Self::Error> {
		S::deserialize_resource(*self, resource)
	}

	fn peek_quads<'p>(
		&'p self,
		pattern: impl Into<LinearQuadPattern<&'p R>>,
	) -> Result<impl Iterator<Item = Result<Quad<&'p R>, Self::Error>>, Self::Error>
	where
		R: 'p,
	{
		S::peek_quads(*self, pattern)
	}

	fn deserialize_quad<'p>(
		&mut self,
		pattern: impl Into<LinearQuadPattern<&'p R>>,
	) -> Result<Option<Quad<R>>, Self::Error>
	where
		R: 'p,
	{
		S::deserialize_quad(*self, pattern)
	}
}

pub trait Error<R>: Sized + Debug + Display {
	fn custom(str: impl ToString) -> Self;

	fn missing_resource() -> Self {
		Self::custom("missing resouce")
	}

	fn unexpected_resource(_resource: &R) -> Self {
		Self::custom("unexpected resource")
	}

	fn missing_quad() -> Self {
		Self::custom("missing quad")
	}
}
