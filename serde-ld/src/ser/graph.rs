use rdf_types::Vocabulary;

use crate::{LexicalRepresentation, SerializeSubject};

// use crate::SerializeSubject;

/// Serialize a Linked-Data graph.
pub trait SerializeGraph<V: Vocabulary, I> {
    fn serialize_graph<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: GraphSerializer<V, I>;
}

pub trait GraphSerializer<V: Vocabulary, I> {
    type Ok;
    type Error;

    fn insert<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + LexicalRepresentation<V, I> + SerializeSubject<V, I>;

    fn end(self) -> Result<Self::Ok, Self::Error>;
}

impl<'a, V: Vocabulary, I, S: GraphSerializer<V, I>> GraphSerializer<V, I> for &'a mut S {
    type Ok = ();
    type Error = S::Error;

    fn insert<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + LexicalRepresentation<V, I> + SerializeSubject<V, I>,
    {
        S::insert(self, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}
