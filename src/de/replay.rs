use rdf_syntax::{IndexedBTreeDataset, Quad, Term};

use crate::LinkedDataDeserializer;

pub struct ReplayDeserializer<D, R = Term> {
	inner: D,
	pending_quads: IndexedBTreeDataset<R>,
	pending_resources: Vec<R>,
	visited_quads: Vec<Quad<R>>,
	visited_resources: Vec<R>,
}

impl<D, R> ReplayDeserializer<D, R> {
	pub fn new(inner: D) -> Self {
		Self {
			inner,
			pending_quads: IndexedBTreeDataset::new(),
			pending_resources: Vec::new(),
			visited_quads: Vec::new(),
			visited_resources: Vec::new(),
		}
	}
}

impl<D, R> ReplayDeserializer<D, R>
where
	R: Clone + Ord,
{
	pub fn reset(&mut self) {
		self.pending_quads.extend(self.visited_quads.drain(..));
		self.pending_resources
			.extend(self.visited_resources.drain(..).rev());
	}
}

impl<R: Clone + Ord, D: LinkedDataDeserializer<R>> LinkedDataDeserializer<R>
	for ReplayDeserializer<D, R>
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
		Ok(self
			.pending_resources
			.iter()
			.rev()
			.map(Ok)
			.chain(self.inner.peek_resources()?))
	}

	fn deserialize_resource(&mut self, resource: Option<R>) -> Result<Option<R>, Self::Error> {
		let resource = match self.pending_resources.pop() {
			Some(a) => match resource {
				Some(b) => {
					if a == b {
						Some(a)
					} else {
						self.pending_resources.push(a);
						return Err(crate::de::Error::unexpected_resource(&b));
					}
				}
				None => Some(a),
			},
			None => self.inner.deserialize_resource(resource)?,
		};

		if let Some(r) = resource.clone() {
			self.visited_resources.push(r);
		}

		Ok(resource)
	}

	fn deserialize_quad<'p>(
		&mut self,
		pattern: impl Into<rdf_syntax::pattern::CanonicalQuadPattern<&'p R>>,
	) -> Result<Option<rdf_syntax::Quad<R>>, Self::Error>
	where
		R: 'p,
	{
		let pattern = pattern.into();
		let quad = match self
			.pending_quads
			.extract_pattern_matching(pattern.clone())
			.next()
		{
			Some(quad) => Some(quad),
			None => self.inner.deserialize_quad(pattern)?,
		};

		if let Some(quad) = quad.clone() {
			self.visited_quads.push(quad);
		}

		Ok(quad)
	}

	fn peek_quads<'p>(
		&'p self,
		pattern: impl Into<rdf_syntax::pattern::CanonicalQuadPattern<&'p R>>,
	) -> Result<impl Iterator<Item = Result<Quad<&'p R>, Self::Error>>, Self::Error>
	where
		R: 'p,
	{
		let pattern = pattern.into();
		Ok(self
			.pending_quads
			.pattern_matching(pattern.clone())
			.map(Ok)
			.chain(self.inner.peek_quads(pattern)?))
	}
}
