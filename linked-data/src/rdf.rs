use core::fmt;

use iref::Iri;
use rdf_types::{
	BlankIdVocabulary, Id, InsertIntoVocabulary, IriVocabulary, IriVocabularyMut,
	LanguageTagVocabulary, LiteralVocabulary, LiteralVocabularyMut, Quad, Term, Vocabulary,
};
use xsd_types::XsdDatatype;

pub type RdfId<V> = Id<<V as IriVocabulary>::Iri, <V as BlankIdVocabulary>::BlankId>;
pub type RdfTerm<V> = Term<RdfId<V>, RdfLiteral<V>>;
pub type RdfQuad<V> = Quad<
	RdfId<V>,
	<V as IriVocabulary>::Iri,
	Term<RdfId<V>, <V as LiteralVocabulary>::Literal>,
	RdfId<V>,
>;

pub enum RdfLiteral<V: IriVocabulary + LanguageTagVocabulary, M = ()> {
	Any(String, rdf_types::literal::Type<V::Iri, V::LanguageTag>),
	Xsd(xsd_types::Value),
	Json(json_syntax::Value<M>),
}

impl<V: IriVocabulary + LanguageTagVocabulary, M> fmt::Display for RdfLiteral<V, M> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Any(v, _) => v.fmt(f),
			Self::Xsd(v) => v.fmt(f),
			Self::Json(v) => v.fmt(f),
		}
	}
}

const RDF_JSON: &Iri = static_iref::iri!("http://www.w3.org/1999/02/22-rdf-syntax-ns#JSON");

impl<V: Vocabulary + IriVocabularyMut + LiteralVocabularyMut, M> InsertIntoVocabulary<V>
	for RdfLiteral<V, M>
where
	V::Value: From<String> + From<json_syntax::Value<M>> + From<xsd_types::Value>,
	V::Type: From<rdf_types::literal::Type<V::Iri, V::LanguageTag>>,
{
	type Inserted = V::Literal;

	fn insert_into_vocabulary(self, vocabulary: &mut V) -> V::Literal {
		match self {
			Self::Any(s, ty) => {
				vocabulary.insert_owned_literal(rdf_types::Literal::new(s.into(), ty.into()))
			}
			Self::Xsd(v) => {
				let ty = rdf_types::literal::Type::Any(vocabulary.insert(v.type_().iri()));
				vocabulary.insert_owned_literal(rdf_types::Literal::new(v.into(), ty.into()))
			}
			Self::Json(v) => {
				let ty = rdf_types::literal::Type::Any(vocabulary.insert(RDF_JSON));
				vocabulary.insert_owned_literal(rdf_types::Literal::new(v.into(), ty.into()))
			}
		}
	}
}
