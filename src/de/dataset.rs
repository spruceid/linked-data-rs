use rdf_syntax::{
	CowTerm, PatternMatchingDatasetMut, Quad, TryInterpretationMut, TryReverseInterpretation,
	pattern::LinearQuadPattern,
};

use crate::LinkedDataDeserializer;

pub struct DatasetDeserializer<D, I> {
	dataset: D,
	interpretation: I,
}

impl<D, I> DatasetDeserializer<D, I> {
	pub fn new(dataset: D, interpretation: I) -> Self {
		Self {
			dataset,
			interpretation,
		}
	}
}

impl<D, I> LinkedDataDeserializer<I::Resource> for DatasetDeserializer<D, I>
where
	D: PatternMatchingDatasetMut<Resource = I::Resource>,
	I: TryReverseInterpretation + TryInterpretationMut,
	I::Resource: Clone,
{
	type Error = DatasetDeserializerError;

	fn interpret(&mut self, term: CowTerm) -> Result<I::Resource, Self::Error> {
		self.interpretation
			.try_insert_term(term)
			.map_err(|e| DatasetDeserializerError(e.to_string()))
	}

	fn terms_of<'a>(
		&'a self,
		resource: &'a I::Resource,
	) -> Result<impl Iterator<Item = Result<CowTerm<'a>, Self::Error>>, Self::Error> {
		Ok(self
			.interpretation
			.try_terms_of(resource)
			.map_err(DatasetDeserializerError::new)?
			.map(|r| r.map(Into::into).map_err(DatasetDeserializerError::new)))
	}

	fn peek_resources<'a>(
		&'a self,
	) -> Result<impl Iterator<Item = Result<&'a I::Resource, Self::Error>>, Self::Error>
	where
		I::Resource: 'a,
	{
		Ok(std::iter::empty())
	}

	fn deserialize_resource(
		&mut self,
		resource: Option<I::Resource>,
	) -> Result<Option<I::Resource>, Self::Error> {
		match resource {
			Some(r) => Err(super::Error::unexpected_resource(&r)),
			None => Ok(None),
		}
	}

	fn peek_quads<'p>(
		&'p self,
		pattern: impl Into<LinearQuadPattern<&'p I::Resource>>,
	) -> Result<impl Iterator<Item = Result<Quad<&'p I::Resource>, Self::Error>>, Self::Error>
	where
		I::Resource: 'p,
	{
		Ok(self.dataset.quad_pattern_matching(pattern.into()).map(Ok))
	}

	fn deserialize_quad<'p>(
		&mut self,
		pattern: impl Into<LinearQuadPattern<&'p I::Resource>>,
	) -> Result<Option<Quad<I::Resource>>, Self::Error>
	where
		I::Resource: 'p,
	{
		Ok(self.dataset.extract_matching_quads(pattern).next())
	}
}

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub struct DatasetDeserializerError(pub String);

impl DatasetDeserializerError {
	pub fn new(e: impl ToString) -> Self {
		Self(e.to_string())
	}
}

impl<R> crate::de::Error<R> for DatasetDeserializerError {
	fn custom(e: impl ToString) -> Self {
		Self(e.to_string())
	}
}
