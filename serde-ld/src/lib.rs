//! This library provides primitive traits to serialize and deserialize
//! Linked-Data types. It is shipped with derive macros (using the `derive`
//! feature) that can automatically implement those primitives for you.
//!
//! # Example
//!
//! ```
//! use iref::IriBuf;
//! use serde_ld::SerializeLd;
//!
//! #[derive(SerializeLd)]
//! #[ld(prefix("ex" = "http://example.org/"))]
//! struct Foo {
//!   #[ld(id)]
//!   id: IriBuf,
//!
//!   #[ld("ex:name")]
//!   name: String,
//!
//!   #[ld("ex:email")]
//!   email: String
//! }
//! ```
use core::fmt;

use iref::Iri;
use rdf_types::{
    BlankIdVocabulary, Id, InsertIntoVocabulary, IriVocabulary, IriVocabularyMut,
    LanguageTagVocabulary, LiteralVocabulary, LiteralVocabularyMut, Quad, Term, Vocabulary,
};
#[cfg(feature = "derive")]
pub use serde_ld_derive::SerializeLd;

#[doc(hidden)]
pub use iref;

#[doc(hidden)]
pub use rdf_types;

mod datatypes;
mod quads;
mod ser;

pub use quads::{to_quads, to_quads_with, QuadSerializer};
pub use ser::*;
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

pub struct AnonymousBinding<'a, T>(pub &'a Iri, pub &'a T);

impl<'a, T> AnonymousBinding<'a, T> {
    pub fn new(iri: &'a Iri, value: &'a T) -> Self {
        Self(iri, value)
    }
}

impl<'a, V: Vocabulary, I, T> LexicalRepresentation<V, I> for AnonymousBinding<'a, T> {
    fn lexical_representation(
        &self,
        _interpretation: &mut I,
        _vocabulary: &mut V,
    ) -> Option<RdfTerm<V>> {
        None
    }
}

impl<'a, V: Vocabulary + IriVocabularyMut, I, T: SerializePredicate<V, I>> SerializeSubject<V, I>
    for AnonymousBinding<'a, T>
{
    fn serialize_subject<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
    where
        S: SubjectSerializer<V, I>,
    {
        serializer.insert(self.0, self.1)?;
        serializer.end()
    }
}

impl<'a, V: Vocabulary + IriVocabularyMut, I, T: SerializePredicate<V, I>> SerializePredicate<V, I>
    for AnonymousBinding<'a, T>
{
    fn serialize_predicate<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
    where
        S: PredicateSerializer<V, I>,
    {
        serializer.insert(self)?;
        serializer.end()
    }
}

impl<'a, V: Vocabulary + IriVocabularyMut, I, T: SerializePredicate<V, I>> SerializeGraph<V, I>
    for AnonymousBinding<'a, T>
{
    fn serialize_graph<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
    where
        S: GraphSerializer<V, I>,
    {
        serializer.insert(self)?;
        serializer.end()
    }
}

impl<'a, V: Vocabulary + IriVocabularyMut, I, T: SerializePredicate<V, I>> SerializeLd<V, I>
    for AnonymousBinding<'a, T>
{
    fn serialize<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer<V, I>,
    {
        serializer.insert_default(self)?;
        serializer.end()
    }
}
