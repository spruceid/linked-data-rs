use crate::RdfUnordered;

use super::{LinkedDataSerializer, SerializeLinkedData};

impl<R: Clone + Ord, T: SerializeLinkedDataUnordered<R>> SerializeLinkedData<R>
	for RdfUnordered<T>
{
	fn serialize_rdf<S>(&self, serializer: S, graph: Option<&R>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		self.0.serialize_rdf_unordered(serializer, graph)
	}
}

pub trait SerializeLinkedDataUnordered<R: Clone + Ord> {
	fn serialize_rdf_unordered<S>(
		&self,
		serializer: S,
		graph: Option<&R>,
	) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>;
}

impl<R: Clone + Ord, T: ?Sized + SerializeLinkedDataUnordered<R>> SerializeLinkedDataUnordered<R>
	for &T
{
	fn serialize_rdf_unordered<S>(
		&self,
		serializer: S,
		graph: Option<&R>,
	) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		T::serialize_rdf_unordered(*self, serializer, graph)
	}
}

impl<R: Clone + Ord, T: SerializeLinkedData<R>> SerializeLinkedDataUnordered<R> for [T] {
	fn serialize_rdf_unordered<S>(
		&self,
		mut serializer: S,
		graph: Option<&R>,
	) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		for item in self {
			item.serialize_rdf(serializer.as_dyn_mut(), graph)?;
		}

		serializer.end()
	}
}

impl<R: Clone + Ord, T: SerializeLinkedData<R>> SerializeLinkedDataUnordered<R> for Vec<T> {
	fn serialize_rdf_unordered<S>(
		&self,
		serializer: S,
		graph: Option<&R>,
	) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		self.as_slice().serialize_rdf_unordered(serializer, graph)
	}
}
