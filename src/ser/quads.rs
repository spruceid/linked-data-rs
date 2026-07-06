use rdf_syntax::{
	ConstGenDomain, GenInterpretation, GroundInterpretation, InterpretationMut, Quad, Term,
	generator::BlankIdGenerator,
};

use crate::{LinkedDataSerializer, SerializeLinkedData, ser::SerializeLinkedDataWith};

pub struct QuadSerializer<I: GroundInterpretation = GenInterpretation<BlankIdGenerator>> {
	interpretation: I,
	quads: Vec<Quad<I::Resource>>,
}

impl<I: GroundInterpretation> QuadSerializer<I> {
	pub fn new(interpretation: I) -> Self {
		Self {
			interpretation,
			quads: Vec::new(),
		}
	}
}

#[derive(Debug, thiserror::Error)]
pub enum QuadSerializationFailed<R = Term> {
	#[error("ambiguous resource")]
	AmbiguousResource(R),

	#[error("{0}")]
	Other(String),
}

impl<R: Clone> super::Error<R> for QuadSerializationFailed<R> {
	fn ambiguous_resource(r: &R) -> Self {
		Self::AmbiguousResource(r.clone())
	}

	fn custom(str: impl ToString) -> Self {
		Self::Other(str.to_string())
	}
}

impl<I> LinkedDataSerializer<I::Resource> for QuadSerializer<I>
where
	I: InterpretationMut + ConstGenDomain,
	I::Resource: Clone + Ord,
{
	type Error = QuadSerializationFailed<I::Resource>;
	type Ok = Vec<Quad<I::Resource>>;

	fn interpret(&mut self, term: Option<rdf_syntax::CowTerm>) -> Result<I::Resource, Self::Error> {
		match term {
			Some(term) => Ok(self.interpretation.insert_term(term)),
			None => Ok(self.interpretation.new_resource()),
		}
	}

	fn serialize_resource(
		&mut self,
		resource: Option<I::Resource>,
	) -> Result<I::Resource, Self::Error> {
		match resource {
			Some(r) => Ok(r),
			None => Ok(self.interpretation.new_resource()),
		}
	}

	fn serialize_quad(&mut self, quad: Quad<I::Resource>) -> Result<(), Self::Error> {
		self.quads.push(quad);
		Ok(())
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(self.quads)
	}
}

/// Serializes the given value to RDF quads.
pub fn to_rdf_quads(
	value: impl SerializeLinkedData,
) -> Result<Vec<Quad<Term>>, QuadSerializationFailed> {
	value.serialize_rdf(
		QuadSerializer::new(GenInterpretation::new(BlankIdGenerator::new())),
		None,
	)
}

/// Serializes the given value to RDF quads, using the given state.
pub fn to_rdf_quads_with<Q>(
	value: impl SerializeLinkedDataWith<Q>,
	state: Q,
) -> Result<Vec<Quad<Term>>, QuadSerializationFailed> {
	value.serialize_rdf_with(
		state,
		QuadSerializer::new(GenInterpretation::new(BlankIdGenerator::new())),
		None,
	)
}

/// Serializes the given value to RDF quads, using a custom interpretation.
pub fn to_rdf_quads_interpretation<I>(
	value: impl SerializeLinkedData<I::Resource>,
	interpretation: I,
) -> Result<Vec<Quad<I::Resource>>, QuadSerializationFailed<I::Resource>>
where
	I: InterpretationMut + ConstGenDomain,
	I::Resource: Clone + Ord,
{
	value.serialize_rdf(QuadSerializer::new(interpretation), None)
}

/// Serializes the given value to RDF quads, using a custom interpretation.
pub fn to_rdf_quads_interpretation_with<Q, I>(
	value: impl SerializeLinkedDataWith<Q, I::Resource>,
	interpretation: I,
	state: Q,
) -> Result<Vec<Quad<I::Resource>>, QuadSerializationFailed<I::Resource>>
where
	I: InterpretationMut + ConstGenDomain,
	I::Resource: Clone + Ord,
{
	value.serialize_rdf_with(state, QuadSerializer::new(interpretation), None)
}
