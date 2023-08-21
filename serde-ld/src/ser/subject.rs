use rdf_types::Vocabulary;

use crate::{LexicalRepresentation, SerializeGraph, SerializePredicate};

/// Serialize a Linked-Data node.
pub trait SerializeSubject<V: Vocabulary, I> {
    fn serialize_subject<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: SubjectSerializer<V, I>;
}

pub trait SubjectSerializer<V: Vocabulary, I> {
    type Ok;
    type Error;

    fn insert<L, T>(&mut self, predicate: &L, value: &T) -> Result<(), Self::Error>
    where
        L: ?Sized + LexicalRepresentation<V, I>,
        T: ?Sized + SerializePredicate<V, I>;

    fn graph<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + LexicalRepresentation<V, I> + SerializeGraph<V, I>;

    fn end(self) -> Result<Self::Ok, Self::Error>;
}

impl<'s, V: Vocabulary, I, S: SubjectSerializer<V, I>> SubjectSerializer<V, I> for &'s mut S {
    type Ok = ();
    type Error = S::Error;

    fn insert<L, T>(&mut self, predicate: &L, value: &T) -> Result<(), Self::Error>
    where
        L: ?Sized + LexicalRepresentation<V, I>,
        T: ?Sized + SerializePredicate<V, I>,
    {
        S::insert(self, predicate, value)
    }

    fn graph<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + LexicalRepresentation<V, I> + SerializeGraph<V, I>,
    {
        S::graph(self, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}
