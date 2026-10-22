use linked_data::{RdfUnordered, SerializeLinkedData, ser::to_rdf_quads};
use rdf_syntax::{IriBuf, iri};

/// A `#[ld(graph)]` field is serialized without being linked to anything
/// (unlike `#[ld(prop = "...")]` or `#[ld(flatten)]`): instead, its content
/// is placed inside the named graph identified by this struct's own
/// subject (here, `id`).
///
/// Note: `#[ld(graph)]` is currently only supported by
/// `SerializeLinkedData`. Deserializing it back is not generally possible,
/// since nothing links the graph's content to `id` for it to be
/// rediscovered; `DeserializeLinkedData` rejects it at compile time.
#[derive(SerializeLinkedData)]
#[ld(prefix("ex" = "http://example.org/"))]
struct Foo {
	#[ld(flatten)]
	id: IriBuf,

	#[ld(graph)]
	values: RdfUnordered<Vec<Bar>>,
}

#[derive(SerializeLinkedData)]
#[ld(prefix("ex" = "http://example.org/"))]
#[ld(type = "ex:Bar")]
struct Bar {
	#[ld(flatten)]
	id: IriBuf,

	#[ld(prop = "ex:value")]
	value: u32,
}

fn main() {
	let value = Foo {
		id: iri!("http://example.org/graph").to_owned(),
		values: RdfUnordered(vec![Bar {
			id: iri!("http://example.org/graph#item").to_owned(),
			value: 12,
		}]),
	};

	let quads = to_rdf_quads(&value).expect("RDF serialization failed");

	for quad in &quads {
		println!("{quad} .")
	}
}
