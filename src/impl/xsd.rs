use crate::{
	DeserializeLinkedData, LinkedDataDeserializer, LinkedDataSerializer, SerializeLinkedData, de,
};
use rdf_syntax::{CowGroundTerm, CowLiteral, CowLiteralType, CowTerm};
use std::borrow::Cow;
use xsd_types::XSD_STRING;

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

macro_rules! serde_xsd_datatype {
    ($($ty:ty: $value:ident),*) => {
        $(
            impl<R: Clone + Ord> SerializeLinkedData<R> for $ty {
                fn serialize_rdf<S>(&self, mut serializer: S, _graph: Option<&R>) -> Result<S::Ok, S::Error>
                    where
                    S: LinkedDataSerializer<R>,
                {
                    let this = serializer.interpret(Some(
                        CowLiteral::new(
                     			Cow::Owned(self.to_string()),
                     			CowLiteralType::Any(Cow::Borrowed(xsd_types::$value)),
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
                				if literal.r#type == xsd_types::$value {
               					if let Ok(value) = literal.value.into_owned().parse() {
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

serde_xsd_datatype! {
	xsd_types::value::Boolean: XSD_BOOLEAN,
	xsd_types::value::UnsignedByte: XSD_UNSIGNED_BYTE,
	xsd_types::value::UnsignedShort: XSD_UNSIGNED_SHORT,
	xsd_types::value::UnsignedInt: XSD_UNSIGNED_INT,
	xsd_types::value::UnsignedLong: XSD_UNSIGNED_LONG,
	xsd_types::value::Byte: XSD_BYTE,
	xsd_types::value::Short: XSD_SHORT,
	xsd_types::value::Int: XSD_INT,
	xsd_types::value::Long: XSD_LONG
}

impl<R: Clone + Ord> SerializeLinkedData<R> for bool {
	fn serialize_rdf<S>(&self, serializer: S, graph: Option<&R>) -> Result<S::Ok, S::Error>
	where
		S: LinkedDataSerializer<R>,
	{
		xsd_types::value::Boolean(*self).serialize_rdf(serializer, graph)
	}
}

impl<R: Clone + Ord> DeserializeLinkedData<R> for bool {
	fn deserialize_rdf<D>(deserializer: D, graph: Option<&R>) -> Result<Self, D::Error>
	where
		D: LinkedDataDeserializer<R>,
	{
		xsd_types::value::Boolean::deserialize_rdf(deserializer, graph).map(Into::into)
	}
}
