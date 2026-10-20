use rdf_syntax::{
	BlankId, BlankIdBuf, CowTerm, GroundTerm, Id, Iri, IriBuf, Literal, LiteralRef, Quad,
	RDF_FIRST, RDF_NIL, RDF_REST, Term,
};

mod flatten;
mod id;
mod properties;
mod quads;
mod unordered;

pub use flatten::*;
pub use id::*;
pub use properties::*;
pub use quads::*;
pub use unordered::*;

pub trait SerializeLinkedDataWith<Q, R: Clone + Ord = Term> {
	fn serialize_rdf_with<S>(
		&self,
		state: Q,
		serializer: S,
		graph: Option<&R>,
	) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>;

	fn serialize_rdf_subjects_with<S>(
		&self,
		state: Q,
		serializer: S,
		predicate: &R,
		object: &R,
		graph: Option<&R>,
	) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		self.serialize_rdf_with(
			state,
			SubjectsSerializer {
				serializer,
				predicate,
				object,
				graph,
			},
			graph,
		)
	}

	fn serialize_rdf_objects_with<S>(
		&self,
		state: Q,
		serializer: S,
		subject: &R,
		predicate: &R,
		graph: Option<&R>,
	) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		self.serialize_rdf_with(
			state,
			ObjectsSerializer {
				serializer,
				subject,
				predicate,
				graph,
			},
			graph,
		)
	}

	/// Serialize quads, ignoring top level resources.
	fn serialize_rdf_graph_with<S>(
		&self,
		state: Q,
		serializer: S,
		graph: Option<&R>,
	) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		self.serialize_rdf_with(state, GraphSerializer { serializer }, graph)
	}
}

pub trait SerializeLinkedData<R: Clone + Ord = Term> {
	fn serialize_rdf<S>(&self, serializer: S, graph: Option<&R>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>;

	fn serialize_rdf_subjects<S>(
		&self,
		serializer: S,
		predicate: &R,
		object: &R,
		graph: Option<&R>,
	) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		self.serialize_rdf(
			SubjectsSerializer {
				serializer,
				predicate,
				object,
				graph,
			},
			graph,
		)
	}

	fn serialize_rdf_objects<S>(
		&self,
		serializer: S,
		subject: &R,
		predicate: &R,
		graph: Option<&R>,
	) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		self.serialize_rdf(
			ObjectsSerializer {
				serializer,
				subject,
				predicate,
				graph,
			},
			graph,
		)
	}

	/// Serialize quads, ignoring top level resources.
	fn serialize_rdf_graph<S>(&self, serializer: S, graph: Option<&R>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		self.serialize_rdf(GraphSerializer { serializer }, graph)
	}
}

struct SubjectsSerializer<'a, R, S> {
	serializer: S,
	predicate: &'a R,
	object: &'a R,
	graph: Option<&'a R>,
}

impl<'a, R: Clone + Ord, S: LinkedDataSerializer<R>> LinkedDataSerializer<R>
	for SubjectsSerializer<'a, R, S>
{
	type Ok = S::Ok;
	type Error = S::Error;

	fn interpret(&mut self, term: Option<CowTerm>) -> Result<R, Self::Error> {
		self.serializer.interpret(term)
	}

	fn serialize_resource(&mut self, subject: Option<R>) -> Result<R, Self::Error> {
		let subject = match subject {
			Some(s) => s,
			None => self.interpret(None)?,
		};

		self.serializer.serialize_quad(Quad(
			subject.clone(),
			self.predicate.clone(),
			self.object.clone(),
			self.graph.cloned(),
		))?;

		Ok(subject)
	}

	fn serialize_quad(&mut self, quad: Quad<R>) -> Result<(), Self::Error> {
		self.serializer.serialize_quad(quad)
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		self.serializer.end()
	}
}

struct ObjectsSerializer<'a, R, S> {
	serializer: S,
	subject: &'a R,
	predicate: &'a R,
	graph: Option<&'a R>,
}

impl<'a, R: Clone + Ord, S: LinkedDataSerializer<R>> LinkedDataSerializer<R>
	for ObjectsSerializer<'a, R, S>
{
	type Ok = S::Ok;
	type Error = S::Error;

	fn interpret(&mut self, term: Option<CowTerm>) -> Result<R, Self::Error> {
		self.serializer.interpret(term)
	}

	fn serialize_resource(&mut self, object: Option<R>) -> Result<R, Self::Error> {
		let object = match object {
			Some(o) => o,
			None => self.interpret(None)?,
		};

		self.serializer.serialize_quad(Quad(
			self.subject.clone(),
			self.predicate.clone(),
			object.clone(),
			self.graph.cloned(),
		))?;

		Ok(object)
	}

	fn serialize_quad(&mut self, quad: Quad<R>) -> Result<(), Self::Error> {
		self.serializer.serialize_quad(quad)
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		self.serializer.end()
	}
}

struct GraphSerializer<S> {
	serializer: S,
}

impl<R: Clone + Ord, S: LinkedDataSerializer<R>> LinkedDataSerializer<R> for GraphSerializer<S> {
	type Ok = S::Ok;
	type Error = S::Error;

	fn interpret(&mut self, term: Option<CowTerm>) -> Result<R, Self::Error> {
		self.serializer.interpret(term)
	}

	fn serialize_resource(&mut self, object: Option<R>) -> Result<R, Self::Error> {
		match object {
			Some(o) => Ok(o),
			None => self.interpret(None),
		}
	}

	fn serialize_quad(&mut self, quad: Quad<R>) -> Result<(), Self::Error> {
		self.serializer.serialize_quad(quad)
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		self.serializer.end()
	}
}

impl<Q, R: Clone + Ord, T: SerializeLinkedDataWith<Q, R>> SerializeLinkedDataWith<Q, R> for &T {
	fn serialize_rdf_with<S>(
		&self,
		state: Q,
		serializer: S,
		graph: Option<&R>,
	) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		T::serialize_rdf_with(*self, state, serializer, graph)
	}
}

impl<R: Clone + Ord, T: SerializeLinkedData<R>> SerializeLinkedData<R> for &T {
	fn serialize_rdf<S>(&self, serializer: S, graph: Option<&R>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		T::serialize_rdf(*self, serializer, graph)
	}
}

impl<Q, R: Clone + Ord, T: SerializeLinkedDataWith<Q, R>> SerializeLinkedDataWith<Q, R>
	for Option<T>
{
	fn serialize_rdf_with<S>(
		&self,
		state: Q,
		serializer: S,
		graph: Option<&R>,
	) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		match self {
			Some(t) => t.serialize_rdf_with(state, serializer, graph),
			None => serializer.end(),
		}
	}
}

impl<R: Clone + Ord, T: SerializeLinkedData<R>> SerializeLinkedData<R> for Option<T> {
	fn serialize_rdf<S>(&self, serializer: S, graph: Option<&R>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		match self {
			Some(t) => t.serialize_rdf(serializer, graph),
			None => serializer.end(),
		}
	}
}

impl<R: Clone + Ord> SerializeLinkedData<R> for Iri {
	fn serialize_rdf<S>(&self, mut serializer: S, _graph: Option<&R>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		let this = serializer.interpret(Some(self.into()))?;
		serializer.serialize_resource(Some(this))?;
		serializer.end()
	}
}

impl<R: Clone + Ord> SerializeLinkedData<R> for IriBuf {
	fn serialize_rdf<S>(&self, serializer: S, graph: Option<&R>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		self.as_iri().serialize_rdf(serializer, graph)
	}
}

impl<R: Clone + Ord> SerializeLinkedData<R> for BlankId {
	fn serialize_rdf<S>(&self, mut serializer: S, _graph: Option<&R>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		let this = serializer.interpret(Some(self.into()))?;
		serializer.serialize_resource(Some(this))?;
		serializer.end()
	}
}

impl<R: Clone + Ord> SerializeLinkedData<R> for BlankIdBuf {
	fn serialize_rdf<S>(&self, serializer: S, graph: Option<&R>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		self.as_blank_id().serialize_rdf(serializer, graph)
	}
}

impl<R: Clone + Ord> SerializeLinkedData<R> for Id {
	fn serialize_rdf<S>(&self, mut serializer: S, _graph: Option<&R>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		let this = serializer.interpret(Some(self.into()))?;
		serializer.serialize_resource(Some(this))?;
		serializer.end()
	}
}

impl<R: Clone + Ord> SerializeLinkedData<R> for Literal {
	fn serialize_rdf<S>(&self, mut serializer: S, _graph: Option<&R>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		let this = serializer.interpret(Some(self.into()))?;
		serializer.serialize_resource(Some(this))?;
		serializer.end()
	}
}

impl<R: Clone + Ord> SerializeLinkedData<R> for LiteralRef<'_> {
	fn serialize_rdf<S>(&self, mut serializer: S, _graph: Option<&R>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		let this = serializer.interpret(Some((*self).into()))?;
		serializer.serialize_resource(Some(this))?;
		serializer.end()
	}
}

impl<R: Clone + Ord> SerializeLinkedData<R> for GroundTerm {
	fn serialize_rdf<S>(&self, mut serializer: S, _graph: Option<&R>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		let this = serializer.interpret(Some(self.clone().into()))?;
		serializer.serialize_resource(Some(this))?;
		serializer.end()
	}
}

impl<R: Clone + Ord> SerializeLinkedData<R> for Term {
	fn serialize_rdf<S>(&self, mut serializer: S, _graph: Option<&R>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		let this = serializer.interpret(Some(self.clone().into()))?;
		serializer.serialize_resource(Some(this))?;
		serializer.end()
	}
}

impl<Q: Clone, R: Clone + Ord, T: SerializeLinkedDataWith<Q, R>> SerializeLinkedDataWith<Q, R>
	for [T]
{
	fn serialize_rdf_with<S>(
		&self,
		state: Q,
		mut serializer: S,
		graph: Option<&R>,
	) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		let prop_first = serializer.interpret(Some(RDF_FIRST.into()))?;
		let prop_rest = serializer.interpret(Some(RDF_REST.into()))?;

		let head = if self.is_empty() {
			Some(serializer.interpret(Some(RDF_NIL.into()))?)
		} else {
			None
		};

		let mut head = serializer.serialize_resource(head)?;

		for (i, value) in self.iter().enumerate() {
			value.serialize_rdf_objects_with(
				state.clone(),
				serializer.as_dyn_mut(),
				&head,
				&prop_first,
				graph,
			)?;

			let rest_term = if i + 1 < self.len() {
				None
			} else {
				Some(RDF_NIL.into())
			};

			let rest = serializer.interpret(rest_term)?;

			serializer.serialize_quad(Quad(
				head,
				prop_rest.clone(),
				rest.clone(),
				graph.cloned(),
			))?;

			head = rest;
		}

		serializer.end()
	}
}

impl<R: Clone + Ord, T: SerializeLinkedData<R>> SerializeLinkedData<R> for [T] {
	fn serialize_rdf<S>(&self, mut serializer: S, graph: Option<&R>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		let prop_first = serializer.interpret(Some(RDF_FIRST.into()))?;
		let prop_rest = serializer.interpret(Some(RDF_REST.into()))?;

		let head = if self.is_empty() {
			Some(serializer.interpret(Some(RDF_NIL.into()))?)
		} else {
			None
		};

		let mut head = serializer.serialize_resource(head)?;

		for (i, value) in self.iter().enumerate() {
			value.serialize_rdf_objects(serializer.as_dyn_mut(), &head, &prop_first, graph)?;

			let rest_term = if i + 1 < self.len() {
				None
			} else {
				Some(RDF_NIL.into())
			};

			let rest = serializer.interpret(rest_term)?;

			serializer.serialize_quad(Quad(
				head,
				prop_rest.clone(),
				rest.clone(),
				graph.cloned(),
			))?;

			head = rest;
		}

		serializer.end()
	}
}

impl<Q: Clone, R: Clone + Ord, T: SerializeLinkedDataWith<Q, R>> SerializeLinkedDataWith<Q, R>
	for Vec<T>
{
	fn serialize_rdf_with<S>(
		&self,
		state: Q,
		serializer: S,
		graph: Option<&R>,
	) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		self.as_slice().serialize_rdf_with(state, serializer, graph)
	}
}

impl<R: Clone + Ord, T: SerializeLinkedData<R>> SerializeLinkedData<R> for Vec<T> {
	fn serialize_rdf<S>(&self, serializer: S, graph: Option<&R>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		self.as_slice().serialize_rdf(serializer, graph)
	}
}

impl<R: Clone + Ord> SerializeLinkedData<R> for String {
	fn serialize_rdf<S>(&self, mut serializer: S, _graph: Option<&R>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		let this = serializer.interpret(Some(self.into()))?;
		serializer.serialize_resource(Some(this))?;
		serializer.end()
	}
}

pub trait LinkedDataSerializer<R: Clone + Ord = Term> {
	type Ok;
	type Error: Error<R>;

	fn as_dyn_mut(&mut self) -> &mut dyn LinkedDataSerializer<R, Ok = Self::Ok, Error = Self::Error>
	where
		Self: Sized,
	{
		self
	}

	fn interpret(&mut self, term: Option<CowTerm>) -> Result<R, Self::Error>;

	fn serialize_resource(&mut self, resource: Option<R>) -> Result<R, Self::Error>;

	fn serialize_quad(&mut self, quad: Quad<R>) -> Result<(), Self::Error>;

	fn end(self) -> Result<Self::Ok, Self::Error>;
}

impl<R: Clone + Ord, S: ?Sized + LinkedDataSerializer<R>> LinkedDataSerializer<R> for &mut S {
	type Ok = ();
	type Error = S::Error;

	fn interpret(&mut self, term: Option<CowTerm>) -> Result<R, Self::Error> {
		S::interpret(*self, term)
	}

	fn serialize_resource(&mut self, resource: Option<R>) -> Result<R, Self::Error> {
		S::serialize_resource(*self, resource)
	}

	fn serialize_quad(&mut self, quad: Quad<R>) -> Result<(), Self::Error> {
		S::serialize_quad(*self, quad)
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(())
	}
}

pub trait Error<R>: Sized {
	fn custom(str: impl ToString) -> Self;

	fn ambiguous_resource(_r: &R) -> Self {
		Self::custom("ambiguous resouce")
	}
}
