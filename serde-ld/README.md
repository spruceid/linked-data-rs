# Linked-Data serialization and deserialization primitives

<!-- cargo-rdme start -->

This library provides primitive traits to serialize and deserialize
Linked-Data types. It is shipped with derive macros (using the `derive`
feature) that can automatically implement those primitives for you.

## Example

```rust
use iref::IriBuf;
use serde_ld::SerializeLd;

#[derive(SerializeLd)]
#[ld(prefix("ex" = "http://example.org/"))]
struct Foo {
  #[ld(id)]
  id: IriBuf,

  #[ld("ex:name")]
  name: String,

  #[ld("ex:email")]
  email: String
}
```

<!-- cargo-rdme end -->
