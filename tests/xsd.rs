//! Tests of the XSD datatypes support.
use linked_data::{
	DeserializeLinkedData, SerializeLinkedData, de::from_rdf_quads, ser::to_rdf_quads,
};
use rdf_syntax::{GroundTerm, Iri, Literal, Term, blankid, diff::RdfDiff};

fn literal(lexical: &str, datatype: &Iri) -> Term {
	Term::Ground(GroundTerm::Literal(Literal::new(lexical, datatype)))
}

fn quads(lexical: &str, datatype: &Iri) -> Vec<rdf_syntax::GrdfQuad> {
	let document = format!("_:0 <http://example.org/value> \"{lexical}\"^^<{datatype}> .\n");
	nquads_syntax::grdf_document_from_str(&document).unwrap().0
}

/// Generates a test for each datatype, checking that the canonical lexical
/// form is deserialized from, and serialized back to, a literal of the
/// datatype.
macro_rules! round_trip_tests {
	($($name:ident: $ty:ty = $lexical:literal ^^ $datatype:ident;)*) => {
		$(
			mod $name {
				use super::*;

				#[derive(Debug, SerializeLinkedData, DeserializeLinkedData)]
				#[ld(prefix("ex" = "http://example.org/"))]
				struct Wrapper {
					#[ld(prop = "ex:value")]
					value: $ty,
				}

				#[test]
				fn deserialize_literal() {
					let _: $ty = from_rdf_quads([], [literal($lexical, xsd_types::$datatype)])
						.expect("RDF deserialization failed");
				}

				#[test]
				fn round_trip() {
					let value: Wrapper = from_rdf_quads(
						quads($lexical, xsd_types::$datatype),
						[Term::BlankId(blankid!("_:0").to_owned())],
					)
					.expect("RDF deserialization failed");

					let found = to_rdf_quads(&value).expect("RDF serialization failed");
					let diff = RdfDiff::new_dedup(found, quads($lexical, xsd_types::$datatype));
					eprintln!("{}", diff.colored());
					assert!(diff.is_empty())
				}
			}
		)*
	};
}

round_trip_tests! {
	boolean: xsd_types::XsdBoolean = "true" ^^ XSD_BOOLEAN;
	bool: bool = "false" ^^ XSD_BOOLEAN;

	float: xsd_types::XsdFloat = "1.5E0" ^^ XSD_FLOAT;
	float_infinity: xsd_types::XsdFloat = "INF" ^^ XSD_FLOAT;
	double: xsd_types::XsdDouble = "-2.5E3" ^^ XSD_DOUBLE;
	double_negative_infinity: xsd_types::XsdDouble = "-INF" ^^ XSD_DOUBLE;
	decimal: xsd_types::XsdDecimal = "3.14" ^^ XSD_DECIMAL;
	integer: xsd_types::XsdInteger = "-42" ^^ XSD_INTEGER;
	non_positive_integer: xsd_types::XsdNonPositiveInteger = "-3" ^^ XSD_NON_POSITIVE_INTEGER;
	negative_integer: xsd_types::XsdNegativeInteger = "-1" ^^ XSD_NEGATIVE_INTEGER;
	non_negative_integer: xsd_types::XsdNonNegativeInteger = "0" ^^ XSD_NON_NEGATIVE_INTEGER;
	positive_integer: xsd_types::XsdPositiveInteger = "7" ^^ XSD_POSITIVE_INTEGER;
	unsigned_byte: xsd_types::XsdUnsignedByte = "255" ^^ XSD_UNSIGNED_BYTE;
	unsigned_short: xsd_types::XsdUnsignedShort = "65535" ^^ XSD_UNSIGNED_SHORT;
	unsigned_int: xsd_types::XsdUnsignedInt = "4294967295" ^^ XSD_UNSIGNED_INT;
	unsigned_long: xsd_types::XsdUnsignedLong = "18446744073709551615" ^^ XSD_UNSIGNED_LONG;
	byte: xsd_types::XsdByte = "-128" ^^ XSD_BYTE;
	short: xsd_types::XsdShort = "-32768" ^^ XSD_SHORT;
	int: xsd_types::XsdInt = "-2147483648" ^^ XSD_INT;
	long: xsd_types::XsdLong = "-9223372036854775808" ^^ XSD_LONG;

	normalized_string: xsd_types::XsdNormalizedString = "hello world" ^^ XSD_NORMALIZED_STRING;
	token: xsd_types::XsdTokenBuf = "a token" ^^ XSD_TOKEN;
	language: xsd_types::XsdLanguageBuf = "en-US" ^^ XSD_LANGUAGE;
	name: xsd_types::XsdNameBuf = "name" ^^ XSD_NAME;
	nc_name: xsd_types::XsdNCNameBuf = "local" ^^ XSD_NC_NAME;
	id: xsd_types::XsdIdBuf = "id-1" ^^ XSD_ID;
	id_ref: xsd_types::XsdIdRefBuf = "id-1" ^^ XSD_IDREF;
	nm_token: xsd_types::XsdNMTokenBuf = "tok-1" ^^ XSD_NMTOKEN;
	q_name: xsd_types::XsdQNameBuf = "xs:string" ^^ XSD_Q_NAME;

	duration: xsd_types::XsdDuration = "P1Y2M3DT4H5M6S" ^^ XSD_DURATION;
	day_time_duration: xsd_types::XsdDayTimeDuration = "P1DT2H" ^^ XSD_DAY_TIME_DURATION;
	year_month_duration: xsd_types::XsdYearMonthDuration = "P1Y2M" ^^ XSD_YEAR_MONTH_DURATION;
	date_time: xsd_types::XsdDateTime = "2024-01-02T03:04:05Z" ^^ XSD_DATE_TIME;
	date_time_stamp: xsd_types::XsdDateTimeStamp = "2024-01-02T03:04:05Z" ^^ XSD_DATE_TIME_STAMP;
	time: xsd_types::XsdTime = "03:04:05" ^^ XSD_TIME;
	date: xsd_types::XsdDate = "2024-01-02" ^^ XSD_DATE;
	g_year_month: xsd_types::XsdGYearMonth = "2024-01" ^^ XSD_G_YEAR_MONTH;
	g_year: xsd_types::XsdGYear = "2024" ^^ XSD_G_YEAR;
	g_month_day: xsd_types::XsdGMonthDay = "--01-02" ^^ XSD_G_MONTH_DAY;
	g_day: xsd_types::XsdGDay = "---02" ^^ XSD_G_DAY;
	g_month: xsd_types::XsdGMonth = "--01" ^^ XSD_G_MONTH;

	base64_binary: xsd_types::XsdBase64BinaryBuf = "SGVsbG8=" ^^ XSD_BASE64_BINARY;
	hex_binary: xsd_types::XsdHexBinaryBuf = "48656C6C6F" ^^ XSD_HEX_BINARY;
}

#[test]
fn rejects_other_datatype() {
	let result =
		from_rdf_quads::<xsd_types::XsdUnsignedInt>([], [literal("5", xsd_types::XSD_STRING)]);

	assert!(result.is_err());
}

#[test]
fn rejects_invalid_lexical_form() {
	let result = from_rdf_quads::<xsd_types::XsdUnsignedInt>(
		[],
		[literal("-5", xsd_types::XSD_UNSIGNED_INT)],
	);

	assert!(result.is_err());
}
