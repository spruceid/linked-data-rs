use linked_data::{de::from_rdf_quads, ser::to_rdf_quads};
use linked_data_derive::{DeserializeLinkedData, SerializeLinkedData};
use rdf_syntax::{IriBuf, iri};

#[derive(Debug, PartialEq, SerializeLinkedData, DeserializeLinkedData)]
#[ld(prefix("ex" = "http://example.org/"))]
struct Foo {
	#[ld(flatten)]
	id: IriBuf,

	#[ld(prop = "ex:name")]
	name: String,

	#[ld(prop = "ex:bar")]
	bar: Bar,

	#[ld(flatten)]
	more: MoreFoo,

	#[ld(prop = "ex:recurse")]
	recurse: Vec<Foo>,
}

#[derive(Debug, PartialEq, SerializeLinkedData, DeserializeLinkedData)]
#[ld(prefix("ex" = "http://example.org/"))]
#[ld(type = "http://example.org/Bar")]
struct Bar {
	#[ld(flatten)]
	id: IriBuf,

	#[ld(prop = "ex:value")]
	value: u32,
}

#[derive(Debug, PartialEq, SerializeLinkedData, DeserializeLinkedData)]
#[ld(prefix("ex" = "http://example.org/"))]
struct MoreFoo {
	#[ld(prop = "ex:email")]
	email: String,
}

fn main() {
	let value = Foo {
		id: iri!("http://example.org/john.smith").to_owned(),
		name: "John Smith".to_string(),
		bar: Bar {
			id: iri!("http://example.org/myBar").to_owned(),
			value: 1,
		},
		more: MoreFoo {
			email: "john.smith@example.org".to_string(),
		},
		recurse: Vec::new(),
	};

	let quads = to_rdf_quads(&value).expect("RDF serialization failed");

	for quad in &quads {
		println!("{quad} .")
	}

	let value2: Foo = from_rdf_quads(quads, [value.id.clone().into()]).unwrap();

	assert_eq!(value, value2);
}
