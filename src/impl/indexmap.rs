use indexmap::IndexSet;

use crate::{SerializeLinkedData, ser::SerializeLinkedDataWith};

impl<Q: Clone, R: Clone + Ord, T: SerializeLinkedDataWith<Q, R>> SerializeLinkedDataWith<Q, R>
	for IndexSet<T>
{
	fn serialize_rdf_with<S>(
		&self,
		state: Q,
		mut serializer: S,
		graph: Option<&R>,
	) -> Result<S::Ok, S::Error>
	where
		S: crate::LinkedDataSerializer<R>,
	{
		for item in self {
			item.serialize_rdf_with(state.clone(), serializer.as_dyn_mut(), graph)?;
		}

		serializer.end()
	}
}

impl<R: Clone + Ord, T: SerializeLinkedData<R>> SerializeLinkedData<R> for IndexSet<T> {
	fn serialize_rdf<S>(&self, mut serializer: S, graph: Option<&R>) -> Result<S::Ok, S::Error>
	where
		S: crate::LinkedDataSerializer<R>,
	{
		for item in self {
			item.serialize_rdf(serializer.as_dyn_mut(), graph)?;
		}

		serializer.end()
	}
}
