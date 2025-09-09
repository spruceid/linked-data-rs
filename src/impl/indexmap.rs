use indexmap::IndexSet;

use crate::SerializeLinkedData;

impl<R, T: SerializeLinkedData<R>> SerializeLinkedData<R> for IndexSet<T> {
	fn serialize_rdf<S>(&self, mut serializer: S, graph: Option<&R>) -> Result<S::Ok, S::Error>
	where
		S: crate::LinkedDataSerializer<R>,
	{
		for item in self {
			item.serialize_rdf(&mut serializer, graph)?;
		}

		serializer.end()
	}
}
