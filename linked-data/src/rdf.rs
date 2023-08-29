use core::fmt;

use educe::Educe;
use iref::Iri;
use rdf_types::{
	BlankIdVocabulary, Id, InsertIntoVocabulary, Interpretation, IriVocabulary, IriVocabularyMut,
	LanguageTagVocabulary, LiteralVocabulary, LiteralVocabularyMut, Quad, Term, Vocabulary,
};
use xsd_types::XsdDatatype;

pub type RdfId<V> = Id<<V as IriVocabulary>::Iri, <V as BlankIdVocabulary>::BlankId>;
pub type RdfTerm<V> = Term<RdfId<V>, RdfLiteral<V>>;

pub type RdfIdRef<'a, V> = Id<&'a <V as IriVocabulary>::Iri, &'a <V as BlankIdVocabulary>::BlankId>;
pub type RdfTermRef<'a, V> = Term<RdfIdRef<'a, V>, RdfLiteralRef<'a, V>>;

pub type RdfQuad<V> = Quad<
	RdfId<V>,
	<V as IriVocabulary>::Iri,
	Term<RdfId<V>, <V as LiteralVocabulary>::Literal>,
	RdfId<V>,
>;

pub type InterpretedQuad<I> = Quad<
	<I as Interpretation>::Resource,
	<I as Interpretation>::Resource,
	<I as Interpretation>::Resource,
	<I as Interpretation>::Resource,
>;

#[derive(Educe)]
#[educe(Debug(bound = "V::Iri: fmt::Debug, V::BlankId: fmt::Debug, V::LanguageTag: fmt::Debug"))]
pub enum CowRdfTerm<'a, V: Vocabulary> {
	Borrowed(RdfTermRef<'a, V>),
	Owned(RdfTerm<V>),
}

impl<'a, V: Vocabulary> CowRdfTerm<'a, V> {
	pub fn as_term_ref(&self) -> RdfTermRef<V> {
		match self {
			Self::Borrowed(t) => *t,
			Self::Owned(t) => match t {
				Term::Id(Id::Iri(i)) => Term::Id(Id::Iri(i)),
				Term::Id(Id::Blank(b)) => Term::Id(Id::Blank(b)),
				Term::Literal(l) => Term::Literal(l.as_literal_ref()),
			},
		}
	}

	pub fn into_owned(self) -> RdfTerm<V>
	where
		V::Iri: Clone,
		V::BlankId: Clone,
		V::LanguageTag: Clone,
	{
		match self {
			Self::Borrowed(t) => match t {
				Term::Id(Id::Iri(i)) => Term::Id(Id::Iri(i.clone())),
				Term::Id(Id::Blank(b)) => Term::Id(Id::Blank(b.clone())),
				Term::Literal(l) => Term::Literal(l.into_owned()),
			},
			Self::Owned(t) => t,
		}
	}
}

#[derive(Educe)]
#[educe(Debug(bound = "V::Iri: fmt::Debug, V::LanguageTag: fmt::Debug, M: fmt::Debug"))]
pub enum RdfLiteral<V: IriVocabulary + LanguageTagVocabulary, M = ()> {
	Any(String, rdf_types::literal::Type<V::Iri, V::LanguageTag>),
	Xsd(xsd_types::Value),
	Json(json_syntax::Value<M>),
}

impl<V: IriVocabulary + LanguageTagVocabulary, M> RdfLiteral<V, M> {
	pub fn as_literal_ref(&self) -> RdfLiteralRef<V, M> {
		match self {
			Self::Any(value, ty) => {
				let ty = match ty {
					rdf_types::literal::Type::Any(i) => rdf_types::literal::Type::Any(i),
					rdf_types::literal::Type::LangString(t) => {
						rdf_types::literal::Type::LangString(t)
					}
				};

				RdfLiteralRef::Any(value, ty)
			}
			Self::Xsd(value) => RdfLiteralRef::Xsd(value.as_value_ref()),
			Self::Json(value) => RdfLiteralRef::Json(value),
		}
	}
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

#[derive(Educe)]
#[educe(
	Debug(bound = "V::Iri: fmt::Debug, V::LanguageTag: fmt::Debug, M: fmt::Debug"),
	Clone,
	Copy
)]
pub enum RdfLiteralRef<'a, V: IriVocabulary + LanguageTagVocabulary, M = ()> {
	Any(
		&'a str,
		rdf_types::literal::Type<&'a V::Iri, &'a V::LanguageTag>,
	),
	Xsd(xsd_types::ValueRef<'a>),
	Json(&'a json_syntax::Value<M>),
}

impl<'a, V: IriVocabulary + LanguageTagVocabulary, M> RdfLiteralRef<'a, V, M> {
	pub fn into_owned(self) -> RdfLiteral<V, M>
	where
		V::Iri: Clone,
		V::LanguageTag: Clone,
		M: Clone,
	{
		match self {
			Self::Any(value, ty) => {
				let ty = match ty {
					rdf_types::literal::Type::Any(iri) => {
						rdf_types::literal::Type::Any(iri.clone())
					}
					rdf_types::literal::Type::LangString(tag) => {
						rdf_types::literal::Type::LangString(tag.clone())
					}
				};

				RdfLiteral::Any(value.to_owned(), ty)
			}
			Self::Xsd(value) => RdfLiteral::Xsd(value.into_owned()),
			Self::Json(value) => RdfLiteral::Json(value.clone()),
		}
	}
}
