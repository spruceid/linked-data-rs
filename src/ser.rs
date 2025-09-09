use rdf_types::{BlankIdBuf, GroundTerm, Id, IriBuf, Quad, Term};

pub trait SerializeLinkedData<R = Term> {
	fn serialize_rdf<S>(&self, serializer: S, graph: Option<&R>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>;

	fn serialize_rdf_objects<S>(
		&self,
		serializer: &mut S,
		graph: Option<&R>,
		subject: &R,
		predicate: &R,
	) -> Result<(), S::Error>
	where
		R: Clone,
		S: LinkedDataSerializer<R>,
	{
		struct ObjectsSerializer<'a, R, S> {
			serializer: S,
			subject: &'a R,
			predicate: &'a R,
			graph: Option<&'a R>,
		}

		impl<'a, R: Clone, S: LinkedDataSerializer<R>> LinkedDataSerializer<R>
			for ObjectsSerializer<'a, R, S>
		{
			type Ok = S::Ok;
			type Error = S::Error;

			fn new_resource(&mut self) -> Result<R, Self::Error> {
				self.serializer.new_resource()
			}

			fn serialize_resource(&mut self, object: R) -> Result<(), Self::Error> {
				self.serializer.serialize_quad(Quad(
					self.subject.clone(),
					self.predicate.clone(),
					object,
					self.graph.cloned(),
				))
			}

			fn serialize_quad(&mut self, quad: Quad<R>) -> Result<(), Self::Error> {
				self.serializer.serialize_quad(quad)
			}

			fn end(self) -> Result<Self::Ok, Self::Error> {
				self.serializer.end()
			}
		}

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
}

impl<T: SerializeLinkedData> SerializeLinkedData for &T {
	fn serialize_rdf<S>(&self, serializer: S, graph: Option<&Term>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<Term>,
	{
		T::serialize_rdf(*self, serializer, graph)
	}
}

impl<T: SerializeLinkedData> SerializeLinkedData for Option<T> {
	fn serialize_rdf<S>(&self, serializer: S, graph: Option<&Term>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<Term>,
	{
		match self {
			Some(t) => t.serialize_rdf(serializer, graph),
			None => serializer.end(),
		}
	}
}

impl SerializeLinkedData for IriBuf {
	fn serialize_rdf<S>(&self, mut serializer: S, _graph: Option<&Term>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<Term>,
	{
		serializer.serialize_resource(self.clone().into())?;
		serializer.end()
	}
}

impl SerializeLinkedData for BlankIdBuf {
	fn serialize_rdf<S>(&self, mut serializer: S, _graph: Option<&Term>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<Term>,
	{
		serializer.serialize_resource(self.clone().into())?;
		serializer.end()
	}
}

impl SerializeLinkedData for Id {
	fn serialize_rdf<S>(&self, mut serializer: S, _graph: Option<&Term>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<Term>,
	{
		serializer.serialize_resource(self.clone().into())?;
		serializer.end()
	}
}

impl SerializeLinkedData for GroundTerm {
	fn serialize_rdf<S>(&self, mut serializer: S, _graph: Option<&Term>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<Term>,
	{
		serializer.serialize_resource(self.clone().into())?;
		serializer.end()
	}
}

impl SerializeLinkedData for Term {
	fn serialize_rdf<S>(&self, mut serializer: S, _graph: Option<&Term>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<Term>,
	{
		serializer.serialize_resource(self.clone())?;
		serializer.end()
	}
}

impl<T: SerializeLinkedData> SerializeLinkedData for [T] {
	fn serialize_rdf<S>(&self, mut serializer: S, graph: Option<&Term>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<Term>,
	{
		for t in self {
			t.serialize_rdf(&mut serializer, graph)?;
		}

		serializer.end()
	}
}

impl<T: SerializeLinkedData> SerializeLinkedData for Vec<T> {
	fn serialize_rdf<S>(&self, serializer: S, graph: Option<&Term>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<Term>,
	{
		self.as_slice().serialize_rdf(serializer, graph)
	}
}

pub trait SerializeLinkedDataProperties<R = Term> {
	fn serialize_rdf_properties<S>(
		&self,
		serializer: S,
		graph: Option<&R>,
		subject: &R,
	) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>;

	fn serialize_rdf_reverse_properties<S>(
		&self,
		serializer: S,
		graph: Option<&R>,
		object: &R,
	) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		struct ReverseSerializer<S>(S);

		impl<R, S: LinkedDataSerializer<R>> LinkedDataSerializer<R> for ReverseSerializer<S> {
			type Ok = S::Ok;
			type Error = S::Error;

			fn new_resource(&mut self) -> Result<R, Self::Error> {
				self.0.new_resource()
			}

			fn serialize_resource(&mut self, resource: R) -> Result<(), Self::Error> {
				self.0.serialize_resource(resource)
			}

			fn serialize_quad(&mut self, Quad(s, p, o, g): Quad<R>) -> Result<(), Self::Error> {
				self.0.serialize_quad(Quad(o, p, s, g))
			}

			fn end(self) -> Result<Self::Ok, Self::Error> {
				self.0.end()
			}
		}

		self.serialize_rdf_properties(ReverseSerializer(serializer), graph, object)
	}
}

impl<R, T: SerializeLinkedDataProperties<R>> SerializeLinkedDataProperties<R> for &T {
	fn serialize_rdf_properties<S>(
		&self,
		serializer: S,
		graph: Option<&R>,
		subject: &R,
	) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		T::serialize_rdf_properties(*self, serializer, graph, subject)
	}
}

impl<R, T: SerializeLinkedDataProperties<R>> SerializeLinkedDataProperties<R> for Option<T> {
	fn serialize_rdf_properties<S>(
		&self,
		serializer: S,
		graph: Option<&R>,
		subject: &R,
	) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		match self {
			Some(t) => t.serialize_rdf_properties(serializer, graph, subject),
			None => serializer.end(),
		}
	}
}

pub trait LinkedDataSerializer<R = Term> {
	type Ok;
	type Error;

	fn new_resource(&mut self) -> Result<R, Self::Error>;

	fn serialize_resource(&mut self, resource: R) -> Result<(), Self::Error>;

	fn serialize_quad(&mut self, quad: Quad<R>) -> Result<(), Self::Error>;

	fn end(self) -> Result<Self::Ok, Self::Error>;
}

impl<R, S: LinkedDataSerializer<R>> LinkedDataSerializer<R> for &mut S {
	type Ok = ();
	type Error = S::Error;

	fn new_resource(&mut self) -> Result<R, Self::Error> {
		S::new_resource(*self)
	}

	fn serialize_resource(&mut self, resource: R) -> Result<(), Self::Error> {
		S::serialize_resource(*self, resource)
	}

	fn serialize_quad(&mut self, quad: Quad<R>) -> Result<(), Self::Error> {
		S::serialize_quad(*self, quad)
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(())
	}
}
