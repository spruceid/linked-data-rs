use rdf_types::Vocabulary;

use crate::{LexicalRepresentation, SerializeSubject};

/// Serialize a Linked-Data predicate values.
pub trait SerializePredicate<V: Vocabulary, I> {
    fn serialize_predicate<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: PredicateSerializer<V, I>;
}

impl<'a, V: Vocabulary, I, T: SerializePredicate<V, I>> SerializePredicate<V, I> for &'a T {
    fn serialize_predicate<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: PredicateSerializer<V, I>,
    {
        T::serialize_predicate(self, serializer)
    }
}

pub trait PredicateSerializer<V: Vocabulary, I> {
    type Ok;
    type Error;

    fn insert<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + LexicalRepresentation<V, I> + SerializeSubject<V, I>;

    fn end(self) -> Result<Self::Ok, Self::Error>;
}
