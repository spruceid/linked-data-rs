use iref::IriBuf;
use linked_data::{to_quads, Deserialize, Serialize};
use rdf_types::{generator, RdfDisplay};

#[derive(Serialize, Deserialize)]
#[ld(prefix("ex" = "http://example.org/"))]
struct Foo {
	#[ld("ex:graph", graph)]
	graph: GraphItem,
}

#[derive(Serialize, Deserialize)]
#[ld(prefix("ex" = "http://example.org/"))]
#[ld(type = "http://example.org/Bar")]
struct GraphItem {
	#[ld(id)]
	id: IriBuf,

	#[ld("ex:value")]
	value: u32,
}

fn main() {
	let value = Foo {
		graph: GraphItem {
			id: IriBuf::new("http://example.org/graph#item".to_string()).unwrap(),
			value: 12,
		},
	};

	let quads = to_quads(generator::Blank::new(), &value).expect("RDF serialization failed");

	for quad in quads {
		println!("{} .", quad.rdf_display())
	}
}
