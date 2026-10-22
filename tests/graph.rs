use linked_data::{RdfUnordered, SerializeLinkedData, ser::to_rdf_quads};
use rdf_syntax::{IriBuf, diff::RdfDiff, iri};

#[derive(Debug, PartialEq, SerializeLinkedData)]
#[ld(prefix("ex" = "http://example.org/"))]
struct MyGraph {
	#[ld(flatten)]
	id: IriBuf,

	#[ld(prop = "ex:name")]
	name: String,

	#[ld(graph)]
	values: RdfUnordered<Vec<Value>>,
}

#[derive(Debug, PartialEq, SerializeLinkedData)]
#[ld(prefix("ex" = "http://example.org/"))]
#[ld(type = "ex:Value")]
struct Value {
	#[ld(flatten)]
	id: IriBuf,

	#[ld(prop = "ex:n")]
	n: u32,
}

#[test]
fn serialize_graph_field() {
	let value = MyGraph {
		id: iri!("http://example.org/myGraph").to_owned(),
		name: "My Graph".to_owned(),
		values: RdfUnordered(vec![
			Value {
				id: iri!("http://example.org/a").to_owned(),
				n: 1,
			},
			Value {
				id: iri!("http://example.org/b").to_owned(),
				n: 2,
			},
		]),
	};

	let found = to_rdf_quads(&value).expect("RDF serialization failed");

	let (expected, _) = nquads_syntax::grdf_document_from_str(include_str!("graph.nq")).unwrap();

	let diff = RdfDiff::new_dedup(found, expected);
	eprintln!("{}", diff.colored());
	assert!(diff.is_empty())
}

#[test]
fn serialize_empty_graph_field() {
	let value = MyGraph {
		id: iri!("http://example.org/myGraph").to_owned(),
		name: "My Graph".to_owned(),
		values: RdfUnordered(Vec::new()),
	};

	let found = to_rdf_quads(&value).expect("RDF serialization failed");

	// An empty `#[ld(graph)]` collection is not linked to anything, so it
	// leaves no trace in the output: only `id`'s own property is emitted.
	assert_eq!(found.len(), 1);
}
