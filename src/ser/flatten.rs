use rdf_syntax::Quad;

use crate::LinkedDataSerializer;

/// Flattened resources serializer.
pub struct FlattenSerializer<'a, R, S> {
	inner: S,
	resources: &'a mut Vec<R>,
	pending: Vec<R>,
}

impl<'a, R, S> FlattenSerializer<'a, R, S> {
	pub fn new(inner: S, resources: &'a mut Vec<R>) -> Self
	where
		R: Clone,
	{
		let mut pending: Vec<R> = resources.clone();
		pending.reverse();

		Self {
			inner,
			resources,
			pending,
		}
	}
}

impl<'a, R: Clone + Ord, S: LinkedDataSerializer<R>> LinkedDataSerializer<R>
	for FlattenSerializer<'a, R, S>
{
	type Error = S::Error;
	type Ok = S::Ok;

	fn interpret(&mut self, term: Option<rdf_syntax::CowTerm>) -> Result<R, Self::Error> {
		self.inner.interpret(term)
	}

	fn serialize_resource(&mut self, resource: Option<R>) -> Result<R, Self::Error> {
		let pending_resource = self.pending.pop();

		match (pending_resource, resource) {
			(Some(a), Some(b)) => {
				if a == b {
					Ok(a)
				} else {
					Err(crate::ser::Error::ambiguous_resource(&a))
				}
			}
			(Some(a), None) => Ok(a),
			(None, b) => {
				let b = self.inner.serialize_resource(b)?;
				self.resources.push(b.clone());
				Ok(b)
			}
		}
	}

	fn serialize_quad(&mut self, quad: Quad<R>) -> Result<(), Self::Error> {
		self.inner.serialize_quad(quad)
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		self.inner.end()
	}
}
