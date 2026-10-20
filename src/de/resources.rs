use crate::LinkedDataDeserializer;

pub struct ResourcesDeserializer<R, D> {
	inner: D,
	resources: Vec<R>,
}

impl<R, D> ResourcesDeserializer<R, D> {
	pub fn new(inner: D, resources: impl IntoIterator<Item = R>) -> Self {
		let mut resources: Vec<R> = resources.into_iter().collect();
		resources.reverse();
		Self { inner, resources }
	}
}

impl<R: Ord, D: LinkedDataDeserializer<R>> LinkedDataDeserializer<R>
	for ResourcesDeserializer<R, D>
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

	fn peek_resources<'a>(
		&'a self,
	) -> Result<impl Iterator<Item = Result<&'a R, Self::Error>>, Self::Error>
	where
		R: 'a,
	{
		Ok(self.resources.iter().rev().map(Ok))
	}

	fn deserialize_resource(&mut self, resource: Option<R>) -> Result<Option<R>, Self::Error> {
		match (resource, self.resources.pop()) {
			(Some(a), Some(b)) => {
				if a == b {
					Ok(Some(a))
				} else {
					self.resources.push(b);
					Err(super::Error::unexpected_resource(&a))
				}
			}
			(Some(a), None) => Err(super::Error::unexpected_resource(&a)),
			(None, Some(a)) => Ok(Some(a)),
			(None, None) => Ok(None),
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
