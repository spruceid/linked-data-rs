use rdf_types::{generator, RdfDisplay};
use serde_ld::{to_quads, LexicalRepresentation, SerializeLd};

#[derive(SerializeLd)]
#[ld(prefix("ex" = "http://example.org/"))]
enum Subject {
	Name(String),
}

#[derive(SerializeLd)]
#[ld(prefix("ex" = "http://example.org/"))]
enum SubjectCompound {
	Name(#[ld("ex:name")] String),
}

#[derive(SerializeLd)]
#[ld(prefix("ex" = "http://example.org/"))]
enum Property {
	#[ld("ex:name")]
	Name(String),
}

#[derive(SerializeLd)]
#[ld(prefix("ex" = "http://example.org/"))]
enum PropertyCompound {
	#[ld("ex:name")]
	Name(#[ld("ex:value")] String),
}

fn main() {
	let a = Subject::Name("A".to_string());
	println!("{}", a.lexical_representation(&mut (), &mut ()).unwrap());

	println!();
	let b = SubjectCompound::Name("B".to_string());
	let quads = to_quads(generator::Blank::new(), &b).expect("RDF serialization failed");
	for quad in quads {
		println!("{} .", quad.rdf_display())
	}

	println!();
	let c = Property::Name("C".to_string());
	let quads = to_quads(generator::Blank::new(), &c).expect("RDF serialization failed");
	for quad in quads {
		println!("{} .", quad.rdf_display())
	}

	println!();
	let d = PropertyCompound::Name("D".to_string());
	let quads = to_quads(generator::Blank::new(), &d).expect("RDF serialization failed");
	for quad in quads {
		println!("{} .", quad.rdf_display())
	}
}
