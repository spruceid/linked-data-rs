use std::sync::LazyLock;

use linked_data::{
	DeserializeLinkedData, SerializeLinkedData, de::from_rdf_quads, ser::to_rdf_quads,
};
use rdf_syntax::{GroundTerm, IriBuf, Literal, Term, blankid, diff::RdfDiff, iri};
use xsd_types::XSD_DECIMAL;

#[derive(Debug, PartialEq, SerializeLinkedData, DeserializeLinkedData)]
#[ld(prefix("ex" = "http://example.org/"))]
struct Struct {
	#[ld(prop = "ex:prop1")]
	prop1: String,

	#[ld(prop = "ex:prop2")]
	prop2: TypedStruct,

	#[ld(prop = "ex:prop3")]
	prop3: Vec<EnumType>,

	#[ld(flatten)]
	flatten: FlattenStructType,
}

#[derive(Debug, PartialEq, SerializeLinkedData, DeserializeLinkedData)]
#[ld(prefix("ex" = "http://example.org/"))]
enum EnumType {
	#[ld(id = "ex:A")]
	A,

	B(String),

	#[ld(id = "ex:C")]
	C {
		#[ld(prop = "ex:c")]
		c: String,
	},

	D {
		#[ld(flatten)]
		id: Term,

		#[ld(prop = "ex:d")]
		d: i32,
	},
}

#[derive(Debug, PartialEq, SerializeLinkedData, DeserializeLinkedData)]
#[ld(prefix("ex" = "http://example.org/"))]
#[ld(type = "ex:Bar")]
struct TypedStruct {
	#[ld(flatten)]
	id: IriBuf,

	#[ld(prop = "ex:value")]
	value: u32,
}

#[derive(Debug, PartialEq, SerializeLinkedData, DeserializeLinkedData)]
#[ld(prefix("ex" = "http://example.org/"))]
struct FlattenStructType {
	#[ld(prop = "ex:flattened")]
	flattened: String,
}

static VALUE: LazyLock<Struct> = LazyLock::new(|| Struct {
	prop1: "John Smith".to_string(),
	prop2: TypedStruct {
		id: iri!("http://example.org/id").to_owned(),
		value: 1,
	},
	prop3: vec![
		EnumType::A,
		EnumType::B("B".to_owned()),
		EnumType::C { c: "C".to_owned() },
		EnumType::D {
			id: Term::Ground(GroundTerm::Literal(Literal::new("123", XSD_DECIMAL))),
			d: 123,
		},
	],
	flatten: FlattenStructType {
		flattened: "flattened".to_string(),
	},
});

#[test]
fn serialize() {
	let found = to_rdf_quads(&*VALUE).expect("RDF serialization failed");

	let (expected, _) = nquads_syntax::grdf_document_from_str(include_str!("derive.nq")).unwrap();

	let diff = RdfDiff::new_dedup(found, expected);
	eprintln!("{}", diff.colored());
	assert!(diff.is_empty())
}

#[test]
fn deserialize() {
	let (quads, _) = nquads_syntax::grdf_document_from_str(include_str!("derive.nq")).unwrap();

	let value: Struct = from_rdf_quads(quads, [Term::BlankId(blankid!("_:0").to_owned())]).unwrap();

	assert_eq!(value, *VALUE)
}
