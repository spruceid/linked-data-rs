use rdf_syntax::Term;

use crate::LinkedDataSerializer;

pub trait SerializeLinkedDataPropertiesWith<Q, R: Clone + Ord = Term> {
	fn serialize_rdf_properties_with<S>(
		&self,
		state: Q,
		serializer: S,
		graph: Option<&R>,
		subject: &R,
	) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>;
}

pub trait SerializeLinkedDataProperties<R: Clone + Ord = Term> {
	fn serialize_rdf_properties<S>(
		&self,
		serializer: S,
		graph: Option<&R>,
		subject: &R,
	) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>;
}

impl<Q, R: Clone + Ord, T: SerializeLinkedDataPropertiesWith<Q, R>>
	SerializeLinkedDataPropertiesWith<Q, R> for &T
{
	fn serialize_rdf_properties_with<S>(
		&self,
		state: Q,
		serializer: S,
		graph: Option<&R>,
		subject: &R,
	) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		T::serialize_rdf_properties_with(*self, state, serializer, graph, subject)
	}
}

impl<R: Clone + Ord, T: SerializeLinkedDataProperties<R>> SerializeLinkedDataProperties<R> for &T {
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

impl<Q, R: Clone + Ord, T: SerializeLinkedDataPropertiesWith<Q, R>>
	SerializeLinkedDataPropertiesWith<Q, R> for Option<T>
{
	fn serialize_rdf_properties_with<S>(
		&self,
		state: Q,
		serializer: S,
		graph: Option<&R>,
		subject: &R,
	) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		match self {
			Some(t) => t.serialize_rdf_properties_with(state, serializer, graph, subject),
			None => serializer.end(),
		}
	}
}

impl<R: Clone + Ord, T: SerializeLinkedDataProperties<R>> SerializeLinkedDataProperties<R>
	for Option<T>
{
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
