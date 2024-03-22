use iref::Iri;
use linked_data::{Deserialize, LinkedDataResource, Serialize};
use static_iref::iri;

#[derive(Serialize, Deserialize)]
enum Foo<'a> {
	Reference(#[ld(id)] &'a Iri),
}

fn main() {
	println!();
	let a = Foo::Reference(iri!("http://example.org/foo"));
	println!("{:?}", a.interpretation(&mut (), &mut ()));
}
