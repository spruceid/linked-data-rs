use rdf_types::{Id, Interpretation, Term, Vocabulary};

use crate::SerializeSubject;

/// Serialize a Linked-Data predicate values.
pub trait SerializePredicate<V: Vocabulary, I: Interpretation> {
    fn serialize_predicate<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: PredicateSerializer<V, I>;
}

pub trait PredicateSerializer<V: Vocabulary, I: Interpretation> {
    type Ok;
    type Error;

    fn vocabulary(&self) -> &V;

    fn vocabulary_mut(&mut self) -> &mut V;

    fn interpretation(&self) -> &I;

    fn interpretation_mut(&mut self) -> &mut I;

    fn insert_nested<T>(
        &mut self,
        term: Term<Id<V::Iri, V::BlankId>, V::Literal>,
        value: &T,
    ) -> Result<(), Self::Error>
    where
        T: ?Sized + SerializeSubject<V, I>;

    fn insert(
        &mut self,
        value: Term<Id<V::Iri, V::BlankId>, V::Literal>,
    ) -> Result<(), Self::Error>;

    fn end(self) -> Result<Self::Ok, Self::Error>;
}
