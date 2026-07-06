use rdf_syntax::{
	GenInterpretation, IndexedBTreeDataset, Quad, Term, TryInterpretationMut,
	TryReverseInterpretation, generator::BlankIdGenerator,
};

use crate::{
	DeserializeLinkedData,
	de::{
		DatasetDeserializer, DatasetDeserializerError, DeserializeLinkedDataWith,
		ResourcesDeserializer,
	},
};

pub type QuadsDeserializationFailed = DatasetDeserializerError;

/// Deserializes an instance of `T` from the given quads.
pub fn from_rdf_quads<T>(
	quads: impl IntoIterator<Item = Quad<Term>>,
	resources: impl IntoIterator<Item = Term>,
) -> Result<T, QuadsDeserializationFailed>
where
	T: DeserializeLinkedData,
{
	from_rdf_quads_interpretation(
		quads,
		resources,
		GenInterpretation::new(BlankIdGenerator::new()),
	)
}

/// Deserializes an instance of `T` from the given quads and state.
pub fn from_rdf_quads_with<Q, T>(
	quads: impl IntoIterator<Item = Quad<Term>>,
	resources: impl IntoIterator<Item = Term>,
	state: Q,
) -> Result<T, QuadsDeserializationFailed>
where
	T: DeserializeLinkedDataWith<Q>,
{
	from_rdf_quads_interpretation_with(
		quads,
		resources,
		GenInterpretation::new(BlankIdGenerator::new()),
		state,
	)
}

/// Deserializes an instance of `T` from the given quads, using a custom RDF
/// [interpretation](rdf_syntax::interpretation).
pub fn from_rdf_quads_interpretation<I, T>(
	quads: impl IntoIterator<Item = Quad<I::Resource>>,
	resources: impl IntoIterator<Item = I::Resource>,
	interpretation: I,
) -> Result<T, QuadsDeserializationFailed>
where
	I: TryInterpretationMut + TryReverseInterpretation,
	I::Resource: Clone + Ord,
	T: DeserializeLinkedData<I::Resource>,
{
	let dataset: IndexedBTreeDataset<I::Resource> = quads.into_iter().collect();
	T::deserialize_rdf(
		ResourcesDeserializer::new(DatasetDeserializer::new(dataset, interpretation), resources),
		None,
	)
}

/// Deserializes an instance of `T` from the given quads, state, and using a
/// custom RDF [interpretation](rdf_syntax::interpretation).
pub fn from_rdf_quads_interpretation_with<Q, I, T>(
	quads: impl IntoIterator<Item = Quad<I::Resource>>,
	resources: impl IntoIterator<Item = I::Resource>,
	interpretation: I,
	state: Q,
) -> Result<T, QuadsDeserializationFailed>
where
	I: TryInterpretationMut + TryReverseInterpretation,
	I::Resource: Clone + Ord,
	T: DeserializeLinkedDataWith<Q, I::Resource>,
{
	let dataset: IndexedBTreeDataset<I::Resource> = quads.into_iter().collect();
	T::deserialize_rdf_with(
		state,
		ResourcesDeserializer::new(DatasetDeserializer::new(dataset, interpretation), resources),
		None,
	)
}
