use rdf_syntax::Quad;

use crate::{LinkedDataSerializer, ser::Error};

pub struct IdSerializer<'a, R, S: ?Sized> {
	inner: &'a mut S,
	resource: Option<R>,
}

impl<'a, R, S: ?Sized> IdSerializer<'a, R, S> {
	pub fn new(inner: &'a mut S) -> Self {
		Self {
			inner,
			resource: None,
		}
	}
}

impl<'a, R: Clone + Ord, S: ?Sized + LinkedDataSerializer<R>> LinkedDataSerializer<R>
	for IdSerializer<'a, R, S>
{
	type Error = S::Error;
	type Ok = Option<R>;

	fn interpret(&mut self, term: Option<rdf_syntax::CowTerm>) -> Result<R, Self::Error> {
		self.inner.interpret(term)
	}

	fn serialize_resource(&mut self, resource: Option<R>) -> Result<R, Self::Error> {
		let r = self.inner.serialize_resource(resource)?;

		if let Some(r) = self.resource.replace(r.clone()) {
			return Err(Error::ambiguous_resource(&r));
		}

		Ok(r)
	}

	fn serialize_quad(&mut self, quad: Quad<R>) -> Result<(), Self::Error> {
		self.inner.serialize_quad(quad)
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(self.resource)
	}
}
