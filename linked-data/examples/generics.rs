use iref::Iri;
use linked_data::{to_quads, Interpret, LinkedData};
use rdf_types::{generator, RdfDisplay};

// #[derive(LinkedData)]
// enum Foo<'a> {
// 	Reference(#[ld(id)] &'a Iri),
// 	Bar(Bar)
// }

// #[derive(LinkedData)]
// struct Bar {
// 	//
// }

#[derive(LinkedData)]
#[ld(prefix("ex" = "http://example.org/"))]
enum Ids {
	#[ld("ex:A")]
	A,
}

fn main() {
	// ...
}
