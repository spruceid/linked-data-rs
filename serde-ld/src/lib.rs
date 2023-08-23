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
use iref::Iri;
use rdf_types::{
    BlankIdVocabulary, Id, IriVocabulary, IriVocabularyMut, LiteralVocabulary, Quad, Term,
    Vocabulary,
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

pub type RdfId<V> = Id<<V as IriVocabulary>::Iri, <V as BlankIdVocabulary>::BlankId>;
pub type RdfTerm<V> = Term<RdfId<V>, <V as LiteralVocabulary>::Literal>;
pub type RdfQuad<V> = Quad<RdfId<V>, <V as IriVocabulary>::Iri, RdfTerm<V>, RdfId<V>>;

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
    ) -> Option<rdf_types::Term<rdf_types::Id<<V>::Iri, <V>::BlankId>, <V>::Literal>> {
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
