use rdf_types::Vocabulary;

mod graph;
mod id;
mod predicate;
mod subject;

pub use graph::*;
pub use id::*;
pub use predicate::*;
pub use subject::*;

/// Serialize a Linked-Data dataset.
pub trait SerializeLd<V: Vocabulary = (), I = ()> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer<V, I>;
}

impl<'a, V: Vocabulary, I, T: SerializeLd<V, I>> SerializeLd<V, I> for &'a T {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer<V, I>
    {
        T::serialize(self, serializer)
    }
}

pub trait Serializer<V: Vocabulary, I> {
    type Ok;
    type Error;

    fn insert_default<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + SerializeGraph<V, I>;

    fn insert<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + LexicalRepresentation<V, I> + SerializeGraph<V, I>;

    fn end(self) -> Result<Self::Ok, Self::Error>;
}

impl<'s, V: Vocabulary, I, S: Serializer<V, I>> Serializer<V, I> for &'s mut S {
    type Ok = ();
    type Error = S::Error;

    fn insert_default<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + SerializeGraph<V, I>,
    {
        S::insert_default(self, value)
    }

    fn insert<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + LexicalRepresentation<V, I> + SerializeGraph<V, I>,
    {
        S::insert(self, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}
