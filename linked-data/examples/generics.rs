use iref::Iri;
use linked_data::{LinkedData, LinkedDataResource};
use static_iref::iri;

#[derive(LinkedData)]
enum Foo<'a> {
	Reference(#[ld(id)] &'a Iri),
}

fn main() {
	println!();
	let a = Foo::Reference(iri!("http://example.org/foo"));
	println!("{:?}", a.interpretation(&mut (), &mut ()));
}
