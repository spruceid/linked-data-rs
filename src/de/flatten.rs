use rdf_syntax::Term;

use crate::LinkedDataDeserializer;

pub struct FlattenDeserializer<'a, D, R = Term> {
	inner: D,
	resources: &'a mut Vec<R>,
	pending: Vec<R>,
}

impl<'a, R, D> FlattenDeserializer<'a, D, R> {
	pub fn new(inner: D, resources: &'a mut Vec<R>) -> Self
	where
		R: Clone,
	{
		let mut pending = resources.clone();
		pending.reverse();

		Self {
			inner,
			resources,
			pending,
		}
	}
}

impl<'a, R: Clone + Ord, D: LinkedDataDeserializer<R>> LinkedDataDeserializer<R>
	for FlattenDeserializer<'a, D, R>
{
	type Error = D::Error;

	fn interpret(&mut self, term: rdf_syntax::CowTerm) -> Result<R, Self::Error> {
		self.inner.interpret(term)
	}

	fn terms_of<'r>(
		&'r self,
		resource: &'r R,
	) -> Result<impl Iterator<Item = Result<rdf_syntax::CowTerm<'r>, Self::Error>>, Self::Error> {
		self.inner.terms_of(resource)
	}

	fn peek_resources<'b>(
		&'b self,
	) -> Result<impl Iterator<Item = Result<&'b R, Self::Error>>, Self::Error>
	where
		R: 'b,
	{
		Ok(self
			.pending
			.iter()
			.rev()
			.map(Ok)
			.chain(self.inner.peek_resources()?))
	}

	fn deserialize_resource(&mut self, resource: Option<R>) -> Result<Option<R>, Self::Error> {
		let pending_resource = self.pending.pop();

		match (pending_resource, resource) {
			(Some(a), Some(b)) => {
				if a == b {
					Ok(Some(a))
				} else {
					Err(crate::de::Error::unexpected_resource(&b))
				}
			}
			(Some(a), None) => Ok(Some(a)),
			(None, b) => Ok(self
				.inner
				.deserialize_resource(b)?
				.inspect(|b| self.resources.push(b.clone()))),
		}
	}

	fn deserialize_quad<'p>(
		&mut self,
		pattern: impl Into<rdf_syntax::pattern::LinearQuadPattern<&'p R>>,
	) -> Result<Option<rdf_syntax::Quad<R>>, Self::Error>
	where
		R: 'p,
	{
		self.inner.deserialize_quad(pattern)
	}

	fn peek_quads<'p>(
		&'p self,
		pattern: impl Into<rdf_syntax::pattern::LinearQuadPattern<&'p R>>,
	) -> Result<impl Iterator<Item = Result<rdf_syntax::Quad<&'p R>, Self::Error>>, Self::Error>
	where
		R: 'p,
	{
		self.inner.peek_quads(pattern)
	}
}
