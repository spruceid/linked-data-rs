use iref::IriBuf;
use serde_ld::SerializeLd;

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
    // ...
}
