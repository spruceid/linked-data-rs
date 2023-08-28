use iref::IriBuf;
use rdf_types::{generator, RdfDisplay};
use serde_ld::{to_quads, SerializeLd};

#[derive(SerializeLd)]
#[ld(prefix("ex" = "http://example.org/"))]
struct Foo {
	#[ld("ex:name")]
	name: String,

	#[ld("ex:bar")]
	bar: Bar,

	#[ld(flatten)]
	more: MoreFoo,
}

#[derive(SerializeLd)]
#[ld(prefix("ex" = "http://example.org/"))]
struct Bar {
	#[ld(id)]
	id: IriBuf,

	#[ld("ex:value")]
	value: u32,
}

#[derive(SerializeLd)]
#[ld(prefix("ex" = "http://example.org/"))]
struct MoreFoo {
	#[ld("ex:email")]
	email: String,
}

fn main() {
	let value = Foo {
		name: "John Smith".to_string(),
		bar: Bar {
			id: IriBuf::new("http://example.org/myBar".to_string()).unwrap(),
			value: 1,
		},
		more: MoreFoo {
			email: "john.smith@example.org".to_string(),
		},
	};

	let quads = to_quads(generator::Blank::new(), &value).expect("RDF serialization failed");

	for quad in quads {
		println!("{} .", quad.rdf_display())
	}
}
