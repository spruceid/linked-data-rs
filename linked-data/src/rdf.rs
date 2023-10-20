use core::fmt;
use std::borrow::Borrow;

use educe::Educe;
use iref::Iri;
use rdf_types::{
	literal, BlankIdVocabulary, Id, InsertIntoVocabulary, Interpretation, IriVocabulary,
	IriVocabularyMut, LanguageTagVocabulary, LiteralVocabulary, LiteralVocabularyMut, Quad, Term,
	Vocabulary,
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
	pub fn from_str(vocabulary: &V, value: &'a str, ty: &'a V::Iri) -> Self {
		use xsd_types::ValueRef;

		let ty_iri = vocabulary.iri(ty).unwrap();

		if ty_iri == RDF_JSON {
			use json_syntax::Parse;
			match json_syntax::Value::parse_str(value, |_| ()) {
				Ok(json) => {
					CowRdfTerm::Owned(RdfTerm::Literal(RdfLiteral::Json(json.into_value())))
				}
				Err(_) => CowRdfTerm::Borrowed(RdfTermRef::Literal(RdfLiteralRef::Any(
					value,
					literal::Type::Any(ty),
				))),
			}
		} else {
			match xsd_types::Datatype::from_iri(ty_iri) {
				Some(xsd_types::Datatype::String(None)) => CowRdfTerm::Borrowed(
					RdfTermRef::Literal(RdfLiteralRef::Xsd(ValueRef::String(value))),
				),
				Some(xsd_ty) => match xsd_ty.parse(value) {
					Ok(xsd_value) => {
						CowRdfTerm::Owned(RdfTerm::Literal(RdfLiteral::Xsd(xsd_value)))
					}
					Err(_) => CowRdfTerm::Borrowed(RdfTermRef::Literal(RdfLiteralRef::Any(
						value,
						literal::Type::Any(ty),
					))),
				},
				None => CowRdfTerm::Borrowed(RdfTermRef::Literal(RdfLiteralRef::Any(
					value,
					literal::Type::Any(ty),
				))),
			}
		}
	}
}

pub enum CowRef<'a, T> {
	Borrowed(&'a T),
	Owned(T),
}

impl<'a, T> AsRef<T> for CowRef<'a, T> {
	fn as_ref(&self) -> &T {
		match self {
			Self::Borrowed(t) => t,
			Self::Owned(t) => t,
		}
	}
}

impl<'a, T> Borrow<T> for CowRef<'a, T> {
	fn borrow(&self) -> &T {
		match self {
			Self::Borrowed(t) => t,
			Self::Owned(t) => t,
		}
	}
}

impl<'a, V: Vocabulary> CowRdfTerm<'a, V> {
	#[allow(clippy::type_complexity)]
	pub fn into_term(
		self,
	) -> Term<Id<CowRef<'a, V::Iri>, CowRef<'a, V::BlankId>>, CowRdfLiteral<'a, V>> {
		match self {
			Self::Borrowed(Term::Id(Id::Iri(i))) => Term::Id(Id::Iri(CowRef::Borrowed(i))),
			Self::Borrowed(Term::Id(Id::Blank(b))) => Term::Id(Id::Blank(CowRef::Borrowed(b))),
			Self::Borrowed(Term::Literal(l)) => Term::Literal(CowRdfLiteral::Borrowed(l)),
			Self::Owned(Term::Id(Id::Iri(i))) => Term::Id(Id::Iri(CowRef::Owned(i))),
			Self::Owned(Term::Id(Id::Blank(b))) => Term::Id(Id::Blank(CowRef::Owned(b))),
			Self::Owned(Term::Literal(l)) => Term::Literal(CowRdfLiteral::Owned(l)),
		}
	}

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

pub trait RdfLiteralValue<M = ()>:
	From<String> + From<xsd_types::Value> + From<json_syntax::Value<M>>
{
}

impl<M, T: From<String> + From<xsd_types::Value> + From<json_syntax::Value<M>>> RdfLiteralValue<M>
	for T
{
}

pub trait AsRdfLiteral<
	V: IriVocabulary + LanguageTagVocabulary + LiteralVocabulary<Value = Self>,
	M = (),
>
{
	fn as_rdf_literal<'a>(&'a self, vocabulary: &V, ty: &'a V::Type) -> CowRdfLiteral<V, M>;
}

impl<
		V: IriVocabulary
			+ LanguageTagVocabulary
			+ LiteralVocabulary<Value = Self, Type = literal::Type<V::Iri, V::LanguageTag>>,
		M,
	> AsRdfLiteral<V, M> for String
{
	fn as_rdf_literal<'a>(
		&'a self,
		_vocabulary: &V,
		ty: &'a <V as LiteralVocabulary>::Type,
	) -> CowRdfLiteral<V, M> {
		let ty = match ty {
			literal::Type::Any(i) => literal::Type::Any(i),
			literal::Type::LangString(t) => literal::Type::LangString(t),
		};

		CowRdfLiteral::Borrowed(RdfLiteralRef::Any(self, ty))
	}
}

pub trait RdfLiteralType<V: IriVocabulary + LanguageTagVocabulary>:
	From<rdf_types::literal::Type<V::Iri, V::LanguageTag>>
{
}

impl<
		V: IriVocabulary + LanguageTagVocabulary,
		T: From<rdf_types::literal::Type<V::Iri, V::LanguageTag>>,
	> RdfLiteralType<V> for T
{
}

#[derive(Educe)]
#[educe(Debug(bound = "V::Iri: fmt::Debug, V::LanguageTag: fmt::Debug, M: fmt::Debug"))]
pub enum RdfLiteral<V: IriVocabulary + LanguageTagVocabulary, M = ()> {
	Any(String, rdf_types::literal::Type<V::Iri, V::LanguageTag>),
	Xsd(xsd_types::Value),
	Json(json_syntax::Value<M>),
}

impl<V: IriVocabulary + LanguageTagVocabulary, M> RdfLiteral<V, M> {
	pub fn into_lexical(self, vocabulary: &V) -> rdf_types::Literal {
		match self {
			Self::Any(s, literal::Type::Any(ty)) => rdf_types::Literal::new(
				s.to_owned(),
				literal::Type::Any(vocabulary.owned_iri(ty).ok().unwrap()),
			),
			Self::Any(s, literal::Type::LangString(lang)) => rdf_types::Literal::new(
				s.to_owned(),
				literal::Type::LangString(vocabulary.owned_language_tag(lang).ok().unwrap()),
			),
			Self::Xsd(value) => {
				let ty = value.type_().iri().to_owned();
				rdf_types::Literal::new(value.to_string(), literal::Type::Any(ty))
			}
			Self::Json(value) => {
				rdf_types::Literal::new(value.to_string(), literal::Type::Any(RDF_JSON.to_owned()))
			}
		}
	}

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
	pub fn into_lexical(self, vocabulary: &V) -> rdf_types::Literal {
		match self {
			Self::Any(s, literal::Type::Any(ty)) => rdf_types::Literal::new(
				s.to_owned(),
				literal::Type::Any(vocabulary.iri(ty).unwrap().to_owned()),
			),
			Self::Any(s, literal::Type::LangString(lang)) => rdf_types::Literal::new(
				s.to_owned(),
				literal::Type::LangString(vocabulary.language_tag(lang).unwrap().cloned()),
			),
			Self::Xsd(value) => {
				let ty = value.type_().iri().to_owned();
				rdf_types::Literal::new(value.to_string(), literal::Type::Any(ty))
			}
			Self::Json(value) => {
				rdf_types::Literal::new(value.to_string(), literal::Type::Any(RDF_JSON.to_owned()))
			}
		}
	}

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

pub enum CowRdfLiteral<'a, V: IriVocabulary + LanguageTagVocabulary, M = ()> {
	Borrowed(RdfLiteralRef<'a, V, M>),
	Owned(RdfLiteral<V, M>),
}

impl<'a, V: IriVocabulary + LanguageTagVocabulary, M> CowRdfLiteral<'a, V, M> {
	pub fn into_owned(self) -> RdfLiteral<V, M>
	where
		V::Iri: Clone,
		V::LanguageTag: Clone,
		M: Clone,
	{
		match self {
			Self::Borrowed(l) => l.into_owned(),
			Self::Owned(l) => l,
		}
	}

	pub fn as_literal_ref(&self) -> RdfLiteralRef<V, M> {
		match self {
			Self::Borrowed(l) => *l,
			Self::Owned(l) => l.as_literal_ref(),
		}
	}
}
