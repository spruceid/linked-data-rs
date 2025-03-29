use core::fmt;
use std::borrow::Borrow;

use educe::Educe;
use iref::Iri;
use rdf_types::{
	vocabulary::{
		BlankIdVocabulary, EmbedIntoVocabulary, IriVocabulary, IriVocabularyMut, LiteralVocabulary,
		LiteralVocabularyMut,
	},
	Id, Interpretation, LiteralType, LiteralTypeRef, Quad, Term, Vocabulary,
};

pub type RdfId<V> = Id<<V as IriVocabulary>::Iri, <V as BlankIdVocabulary>::BlankId>;
pub type RdfTerm<V> = Term<RdfId<V>, RdfLiteral<V>>;

pub type RdfIdRef<'a, V> = Id<&'a <V as IriVocabulary>::Iri, &'a <V as BlankIdVocabulary>::BlankId>;
pub type RdfTermRef<'a, V> = Term<RdfIdRef<'a, V>, RdfLiteralRef<'a, V>>;

pub type RdfQuad<V = ()> = Quad<
	RdfId<V>,
	<V as IriVocabulary>::Iri,
	Term<RdfId<V>, <V as LiteralVocabulary>::Literal>,
	RdfId<V>,
>;

pub type InterpretedQuad<I> = Quad<<I as Interpretation>::Resource>;

#[derive(Educe)]
#[educe(Debug(bound = "V::Iri: fmt::Debug, V::BlankId: fmt::Debug"))]
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
			match json_syntax::Value::parse_str(value) {
				Ok((json, _)) => CowRdfTerm::Owned(RdfTerm::Literal(RdfLiteral::Json(json))),
				Err(_) => CowRdfTerm::Borrowed(RdfTermRef::Literal(RdfLiteralRef::Any(
					value,
					LiteralTypeRef::Any(ty),
				))),
			}
		} else {
			match xsd_types::Datatype::from_iri(ty_iri) {
				Some(xsd_types::Datatype::String(xsd_types::StringDatatype::String)) => {
					CowRdfTerm::Borrowed(RdfTermRef::Literal(RdfLiteralRef::Xsd(ValueRef::String(
						value,
					))))
				}
				Some(xsd_ty) => match xsd_ty.parse(value) {
					Ok(xsd_value) => {
						CowRdfTerm::Owned(RdfTerm::Literal(RdfLiteral::Xsd(xsd_value)))
					}
					Err(_) => CowRdfTerm::Borrowed(RdfTermRef::Literal(RdfLiteralRef::Any(
						value,
						LiteralTypeRef::Any(ty),
					))),
				},
				None => CowRdfTerm::Borrowed(RdfTermRef::Literal(RdfLiteralRef::Any(
					value,
					LiteralTypeRef::Any(ty),
				))),
			}
		}
	}
}

pub enum CowRef<'a, T> {
	Borrowed(&'a T),
	Owned(T),
}

impl<T> AsRef<T> for CowRef<'_, T> {
	fn as_ref(&self) -> &T {
		match self {
			Self::Borrowed(t) => t,
			Self::Owned(t) => t,
		}
	}
}

impl<T> Borrow<T> for CowRef<'_, T> {
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

pub trait RdfLiteralValue:
	From<String> + From<xsd_types::Value> + From<json_syntax::Value>
{
}

impl<T: From<String> + From<xsd_types::Value> + From<json_syntax::Value>> RdfLiteralValue for T {}

pub trait AsRdfLiteral<V: IriVocabulary + LiteralVocabulary> {
	fn as_rdf_literal<'a>(
		&'a self,
		vocabulary: &V,
		ty: LiteralTypeRef<'a, V::Iri>,
	) -> CowRdfLiteral<'a, V>;
}

impl<V: IriVocabulary + LiteralVocabulary> AsRdfLiteral<V> for str {
	fn as_rdf_literal<'a>(
		&'a self,
		_vocabulary: &V,
		ty: LiteralTypeRef<'a, V::Iri>,
	) -> CowRdfLiteral<'a, V> {
		CowRdfLiteral::Borrowed(RdfLiteralRef::Any(self, ty))
	}
}

impl<V: IriVocabulary + LiteralVocabulary> AsRdfLiteral<V> for String {
	fn as_rdf_literal<'a>(
		&'a self,
		vocabulary: &V,
		ty: LiteralTypeRef<'a, V::Iri>,
	) -> CowRdfLiteral<'a, V> {
		self.as_str().as_rdf_literal(vocabulary, ty)
	}
}

pub trait RdfLiteralType<V: IriVocabulary>: From<rdf_types::LiteralType<V::Iri>> {}

impl<V: IriVocabulary, T: From<rdf_types::LiteralType<V::Iri>>> RdfLiteralType<V> for T {}

#[derive(Educe)]
#[educe(Debug(bound = "V::Iri: fmt::Debug"))]
pub enum RdfLiteral<V: IriVocabulary> {
	Any(String, rdf_types::LiteralType<V::Iri>),
	Xsd(xsd_types::Value),
	Json(json_syntax::Value),
}

impl<V: IriVocabulary> RdfLiteral<V> {
	pub fn into_lexical(self, vocabulary: &V) -> rdf_types::Literal {
		match self {
			Self::Any(s, LiteralType::Any(ty)) => rdf_types::Literal::new(
				s.to_owned(),
				LiteralType::Any(vocabulary.owned_iri(ty).ok().unwrap()),
			),
			Self::Any(s, LiteralType::LangString(lang)) => {
				rdf_types::Literal::new(s.to_owned(), LiteralType::LangString(lang))
			}
			Self::Xsd(value) => {
				let ty = value.datatype().iri().to_owned();
				rdf_types::Literal::new(value.to_string(), LiteralType::Any(ty))
			}
			Self::Json(mut value) => {
				value.canonicalize();
				rdf_types::Literal::new(value.to_string(), LiteralType::Any(RDF_JSON.to_owned()))
			}
		}
	}

	pub fn as_literal_ref(&self) -> RdfLiteralRef<V> {
		match self {
			Self::Any(value, ty) => RdfLiteralRef::Any(value, ty.as_ref()),
			Self::Xsd(value) => RdfLiteralRef::Xsd(value.as_ref()),
			Self::Json(value) => RdfLiteralRef::Json(value),
		}
	}
}

impl<V: IriVocabulary> fmt::Display for RdfLiteral<V> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Any(v, _) => v.fmt(f),
			Self::Xsd(v) => v.fmt(f),
			Self::Json(v) => v.fmt(f),
		}
	}
}

const RDF_JSON: &Iri = static_iref::iri!("http://www.w3.org/1999/02/22-rdf-syntax-ns#JSON");

impl<V: Vocabulary + IriVocabularyMut + LiteralVocabularyMut> EmbedIntoVocabulary<V>
	for RdfLiteral<V>
{
	type Embedded = V::Literal;

	fn embed_into_vocabulary(self, vocabulary: &mut V) -> V::Literal {
		match self {
			Self::Any(s, ty) => vocabulary.insert_owned_literal(rdf_types::Literal::new(s, ty)),
			Self::Xsd(v) => {
				let ty = rdf_types::LiteralType::Any(vocabulary.insert(v.datatype().iri()));
				vocabulary.insert_owned_literal(rdf_types::Literal::new(v.into(), ty))
			}
			Self::Json(v) => {
				let ty = rdf_types::LiteralType::Any(vocabulary.insert(RDF_JSON));
				vocabulary.insert_owned_literal(rdf_types::Literal::new(v.into(), ty))
			}
		}
	}
}

#[derive(Educe)]
#[educe(Debug(bound = "V::Iri: fmt::Debug"), Clone, Copy)]
pub enum RdfLiteralRef<'a, V: IriVocabulary = ()> {
	Any(&'a str, rdf_types::LiteralTypeRef<'a, V::Iri>),
	Xsd(xsd_types::ValueRef<'a>),
	Json(&'a json_syntax::Value),
}

impl<V: IriVocabulary> RdfLiteralRef<'_, V> {
	pub fn into_lexical(self, vocabulary: &V) -> rdf_types::Literal {
		match self {
			Self::Any(s, LiteralTypeRef::Any(ty)) => rdf_types::Literal::new(
				s.to_owned(),
				LiteralType::Any(vocabulary.iri(ty).unwrap().to_owned()),
			),
			Self::Any(s, LiteralTypeRef::LangString(lang)) => {
				rdf_types::Literal::new(s.to_owned(), LiteralType::LangString(lang.to_owned()))
			}
			Self::Xsd(value) => {
				let ty = value.datatype().iri().to_owned();
				rdf_types::Literal::new(value.to_string(), LiteralType::Any(ty))
			}
			Self::Json(value) => {
				let mut value = value.clone();
				value.canonicalize();
				rdf_types::Literal::new(value.to_string(), LiteralType::Any(RDF_JSON.to_owned()))
			}
		}
	}

	pub fn into_owned(self) -> RdfLiteral<V>
	where
		V::Iri: Clone,
	{
		match self {
			Self::Any(value, ty) => RdfLiteral::Any(value.to_owned(), ty.into_owned()),
			Self::Xsd(value) => RdfLiteral::Xsd(value.into_owned()),
			Self::Json(value) => RdfLiteral::Json(value.clone()),
		}
	}
}

pub enum CowRdfLiteral<'a, V: IriVocabulary = ()> {
	Borrowed(RdfLiteralRef<'a, V>),
	Owned(RdfLiteral<V>),
}

impl<V: IriVocabulary> CowRdfLiteral<'_, V> {
	pub fn into_owned(self) -> RdfLiteral<V>
	where
		V::Iri: Clone,
	{
		match self {
			Self::Borrowed(l) => l.into_owned(),
			Self::Owned(l) => l,
		}
	}

	pub fn as_literal_ref(&self) -> RdfLiteralRef<V> {
		match self {
			Self::Borrowed(l) => *l,
			Self::Owned(l) => l.as_literal_ref(),
		}
	}
}
