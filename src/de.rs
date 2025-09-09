use rdf_types::{dataset::IndexedBTreeDataset, pattern::CanonicalQuadPattern, Quad, Term};

pub trait DeserializeLinkedData<R = Term>: Sized {
	fn deserialize_rdf<D>(deserializer: D, graph: Option<&R>) -> Result<Self, D::Error>
	where
		D: LinkedDataDeserializer<R>;
}

pub trait LinkedDataDeserializer<R = Term> {
	type Error: Error;

	fn deserialize_resource(&mut self, graph: Option<&R>) -> Result<Option<R>, Self::Error>;

	fn peek_quad<'p>(
		&self,
		pattern: impl Into<CanonicalQuadPattern<&'p R>>,
	) -> Result<Option<Quad<&R>>, Self::Error>
	where
		R: 'p;

	fn deserialize_quad<'p>(
		&mut self,
		pattern: impl Into<CanonicalQuadPattern<&'p R>>,
	) -> Result<Option<Quad<R>>, Self::Error>
	where
		R: 'p;
}

impl<R, S: LinkedDataDeserializer<R>> LinkedDataDeserializer<R> for &mut S {
	type Error = S::Error;

	fn deserialize_resource(&mut self, graph: Option<&R>) -> Result<Option<R>, Self::Error> {
		S::deserialize_resource(*self, graph)
	}

	fn peek_quad<'p>(
		&self,
		pattern: impl Into<CanonicalQuadPattern<&'p R>>,
	) -> Result<Option<Quad<&R>>, Self::Error>
	where
		R: 'p,
	{
		S::peek_quad(*self, pattern)
	}

	fn deserialize_quad<'p>(
		&mut self,
		pattern: impl Into<CanonicalQuadPattern<&'p R>>,
	) -> Result<Option<Quad<R>>, Self::Error>
	where
		R: 'p,
	{
		S::deserialize_quad(*self, pattern)
	}
}

impl<R: Clone + Ord> LinkedDataDeserializer<R> for IndexedBTreeDataset<R> {
	type Error = DatasetDeserializerError;

	fn deserialize_resource(&mut self, _: Option<&R>) -> Result<Option<R>, Self::Error> {
		Ok(None)
	}

	fn peek_quad<'p>(
		&self,
		pattern: impl Into<CanonicalQuadPattern<&'p R>>,
	) -> Result<Option<Quad<&R>>, Self::Error>
	where
		R: 'p,
	{
		Ok(self.pattern_matching(pattern.into()).next())
	}

	fn deserialize_quad<'p>(
		&mut self,
		pattern: impl Into<CanonicalQuadPattern<&'p R>>,
	) -> Result<Option<Quad<R>>, Self::Error>
	where
		R: 'p,
	{
		Ok(self.extract_pattern_matching(pattern.into()).next())
	}
}

pub struct DatasetDeserializerError(pub String);

impl Error for DatasetDeserializerError {
	fn custom(e: impl ToString) -> Self {
		Self(e.to_string())
	}
}

pub struct LinkedDataSubjectDeserializer<R, D> {
	deserializer: D,
	subject: Option<R>,
}

impl<R, D> LinkedDataSubjectDeserializer<R, D> {
	pub fn new(deserializer: D, subject: R) -> Self {
		Self {
			deserializer,
			subject: Some(subject),
		}
	}
}

impl<R, D: LinkedDataDeserializer<R>> LinkedDataDeserializer<R>
	for LinkedDataSubjectDeserializer<R, D>
{
	type Error = D::Error;

	fn deserialize_resource(&mut self, _: Option<&R>) -> Result<Option<R>, Self::Error> {
		Ok(self.subject.take())
	}

	fn peek_quad<'p>(
		&self,
		pattern: impl Into<CanonicalQuadPattern<&'p R>>,
	) -> Result<Option<Quad<&R>>, Self::Error>
	where
		R: 'p,
	{
		self.deserializer.peek_quad(pattern)
	}

	fn deserialize_quad<'p>(
		&mut self,
		pattern: impl Into<CanonicalQuadPattern<&'p R>>,
	) -> Result<Option<Quad<R>>, Self::Error>
	where
		R: 'p,
	{
		self.deserializer.deserialize_quad(pattern)
	}
}

pub trait Error: Sized {
	fn custom(str: impl ToString) -> Self;

	fn missing_resource() -> Self {
		Self::custom("missing resouce")
	}

	fn unexpected_resource(resource: impl ToString) -> Self {
		Self::custom(format!("unexpected resource: {}", resource.to_string()))
	}
}
