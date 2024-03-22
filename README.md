# Linked-Data serialization and deserialization primitives

[![GitHub Actions Workflow Status](https://img.shields.io/github/actions/workflow/status/spruceid/linked-data-rs/ci.yml?style=flat-square&logo=github)](https://github.com/spruceid/linked-data-rs/actions)
[![Crate informations](https://img.shields.io/crates/v/linked-data.svg?style=flat-square)](https://crates.io/crates/linked-data)
[![Crates.io MSRV](https://img.shields.io/crates/msrv/linked-data?style=flat-square)](https://crates.io/crates/linked-data)
[![License](https://img.shields.io/crates/l/linked-data.svg?style=flat-square)](https://github.com/spruceid/linked-data-rs#license)
[![Documentation](https://img.shields.io/badge/docs-latest-blue.svg?style=flat-square)](https://docs.rs/linked-data)

<!-- cargo-rdme start -->

This library provides primitive traits to serialize and deserialize
Linked-Data types. It is shipped with derive macros (using the `derive`
feature) that can automatically implement those primitives for you.

## Example

```rust
use iref::IriBuf;
use static_iref::iri;

#[derive(linked_data::Serialize, linked_data::Deserialize)]
#[ld(prefix("ex" = "http://example.org/"))]
struct Foo {
  #[ld(id)]
  id: IriBuf,

  #[ld("ex:name")]
  name: String,

  #[ld("ex:email")]
  email: String
}

let value = Foo {
  id: iri!("http://example.org/JohnSmith").to_owned(),
  name: "John Smith".to_owned(),
  email: "john.smith@example.org".to_owned()
};

let quads = linked_data::to_quads(rdf_types::generator::Blank::new(), &value)
  .expect("RDF serialization failed");

for quad in quads {
  use rdf_types::RdfDisplay;
  println!("{} .", quad.rdf_display())
}
```

This should print the following:
```text
<http://example.org/JohnSmith> <http://example.org/name> "John Smith" .
<http://example.org/JohnSmith> <http://example.org/email> "john.smith@example.org" .
```

<!-- cargo-rdme end -->

## License

Licensed under either of

 * Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any
additional terms or conditions.
