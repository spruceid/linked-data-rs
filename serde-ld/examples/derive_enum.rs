use rdf_types::{generator, RdfDisplay};
use serde_ld::{to_quads, SerializeLd};

#[derive(SerializeLd)]
#[ld(prefix("ex" = "http://example.org/"))]
enum Subject {
    Name(String)
}

#[derive(SerializeLd)]
#[ld(prefix("ex" = "http://example.org/"))]
enum SubjectCompound {
    Name(#[ld("ex:name")] String)
}

#[derive(SerializeLd)]
#[ld(prefix("ex" = "http://example.org/"))]
enum Property {
    #[ld("ex:name")]
    Name(String)
}

#[derive(SerializeLd)]
#[ld(prefix("ex" = "http://example.org/"))]
enum PropertyCompound {
    #[ld("ex:name")]
    Name(#[ld("ex:value")] String)
}

fn main() {
    let a = Subject::Name("A".to_string());
    // let quads = to_quads(generator::Blank::new(), &a).expect("RDF serialization failed");
    // for quad in quads {
    //     println!("{} .", quad.rdf_display())
    // }
}
