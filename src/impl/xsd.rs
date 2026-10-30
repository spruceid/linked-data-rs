use crate::{
	DeserializeLinkedData, LinkedDataDeserializer, LinkedDataSerializer, SerializeLinkedData, de,
};
use rdf_syntax::{CowGroundTerm, CowLiteral, CowLiteralType, CowTerm};
use std::borrow::Cow;
use xsd_types::{XSD_STRING, XsdFromStr};

impl<R: Clone + Ord> DeserializeLinkedData<R> for String {
	fn deserialize_rdf<D>(mut deserializer: D, _: Option<&R>) -> Result<Self, D::Error>
	where
		D: LinkedDataDeserializer<R>,
	{
		let this = deserializer
			.deserialize_resource(None)?
			.ok_or_else(|| de::Error::missing_resource())?;

		for term in deserializer.terms_of(&this)? {
			if let CowTerm::Ground(CowGroundTerm::Literal(literal)) = term? {
				if literal.r#type == XSD_STRING {
					return Ok(literal.value.into_owned());
				}
			}
		}

		Err(de::Error::custom("no valid lexical representation"))
	}
}

/// Implements [`SerializeLinkedData`] and [`DeserializeLinkedData`] for XSD
/// value types, as a literal of the given datatype.
///
/// The types must implement [`XsdValue`](xsd_types::XsdValue), whose `Display`
/// implementation writes the canonical lexical form, and [`XsdFromStr`].
macro_rules! serde_xsd_datatype {
	($($ty:ty: $datatype:ident),* $(,)?) => {
		$(
			impl<R: Clone + Ord> SerializeLinkedData<R> for $ty {
				fn serialize_rdf<S>(
					&self,
					mut serializer: S,
					_graph: Option<&R>,
				) -> Result<S::Ok, S::Error>
				where
					S: LinkedDataSerializer<R>,
				{
					let this = serializer.interpret(Some(
						CowLiteral::new(
							Cow::Owned(self.to_string()),
							CowLiteralType::Any(Cow::Borrowed(xsd_types::$datatype)),
						)
						.into(),
					))?;
					serializer.serialize_resource(Some(this))?;
					serializer.end()
				}
			}

			impl<R: Clone + Ord> DeserializeLinkedData<R> for $ty {
				fn deserialize_rdf<D>(mut deserializer: D, _: Option<&R>) -> Result<Self, D::Error>
				where
					D: LinkedDataDeserializer<R>,
				{
					let this = deserializer
						.deserialize_resource(None)?
						.ok_or_else(|| de::Error::missing_resource())?;

					for term in deserializer.terms_of(&this)? {
						if let CowTerm::Ground(CowGroundTerm::Literal(literal)) = term? {
							if literal.r#type == xsd_types::$datatype {
								if let Ok(value) = <$ty as XsdFromStr>::xsd_from_str(&literal.value) {
									return Ok(value);
								}
							}
						}
					}

					Err(de::Error::custom("no valid lexical representation"))
				}
			}
		)*
	};
}

// `xsd_types::XsdString` (`String`) and `xsd_types::XsdAnyUriBuf` (`IriBuf`)
// are not listed here: they are aliases of types implemented elsewhere.
serde_xsd_datatype! {
	// Boolean.
	xsd_types::XsdBoolean: XSD_BOOLEAN,

	// Numbers.
	xsd_types::XsdFloat: XSD_FLOAT,
	xsd_types::XsdDouble: XSD_DOUBLE,
	xsd_types::XsdDecimal: XSD_DECIMAL,
	xsd_types::XsdInteger: XSD_INTEGER,
	xsd_types::XsdNonPositiveInteger: XSD_NON_POSITIVE_INTEGER,
	xsd_types::XsdNegativeInteger: XSD_NEGATIVE_INTEGER,
	xsd_types::XsdNonNegativeInteger: XSD_NON_NEGATIVE_INTEGER,
	xsd_types::XsdPositiveInteger: XSD_POSITIVE_INTEGER,
	xsd_types::XsdUnsignedByte: XSD_UNSIGNED_BYTE,
	xsd_types::XsdUnsignedShort: XSD_UNSIGNED_SHORT,
	xsd_types::XsdUnsignedInt: XSD_UNSIGNED_INT,
	xsd_types::XsdUnsignedLong: XSD_UNSIGNED_LONG,
	xsd_types::XsdByte: XSD_BYTE,
	xsd_types::XsdShort: XSD_SHORT,
	xsd_types::XsdInt: XSD_INT,
	xsd_types::XsdLong: XSD_LONG,

	// Strings.
	xsd_types::XsdNormalizedString: XSD_NORMALIZED_STRING,
	xsd_types::XsdTokenBuf: XSD_TOKEN,
	xsd_types::XsdLanguageBuf: XSD_LANGUAGE,
	xsd_types::XsdNameBuf: XSD_NAME,
	xsd_types::XsdNCNameBuf: XSD_NC_NAME,
	xsd_types::XsdIdBuf: XSD_ID,
	xsd_types::XsdIdRefBuf: XSD_IDREF,
	xsd_types::XsdNMTokenBuf: XSD_NMTOKEN,
	xsd_types::XsdQNameBuf: XSD_Q_NAME,

	// Durations, dates and times.
	xsd_types::XsdDuration: XSD_DURATION,
	xsd_types::XsdDayTimeDuration: XSD_DAY_TIME_DURATION,
	xsd_types::XsdYearMonthDuration: XSD_YEAR_MONTH_DURATION,
	xsd_types::XsdDateTime: XSD_DATE_TIME,
	xsd_types::XsdDateTimeStamp: XSD_DATE_TIME_STAMP,
	xsd_types::XsdTime: XSD_TIME,
	xsd_types::XsdDate: XSD_DATE,
	xsd_types::XsdGYearMonth: XSD_G_YEAR_MONTH,
	xsd_types::XsdGYear: XSD_G_YEAR,
	xsd_types::XsdGMonthDay: XSD_G_MONTH_DAY,
	xsd_types::XsdGDay: XSD_G_DAY,
	xsd_types::XsdGMonth: XSD_G_MONTH,

	// Binary data.
	xsd_types::XsdBase64BinaryBuf: XSD_BASE64_BINARY,
	xsd_types::XsdHexBinaryBuf: XSD_HEX_BINARY,
}

impl<R: Clone + Ord> SerializeLinkedData<R> for bool {
	fn serialize_rdf<S>(&self, serializer: S, graph: Option<&R>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		xsd_types::XsdBoolean(*self).serialize_rdf(serializer, graph)
	}
}

impl<R: Clone + Ord> DeserializeLinkedData<R> for bool {
	fn deserialize_rdf<D>(deserializer: D, graph: Option<&R>) -> Result<Self, D::Error>
	where
		D: LinkedDataDeserializer<R>,
	{
		xsd_types::XsdBoolean::deserialize_rdf(deserializer, graph).map(Into::into)
	}
}
