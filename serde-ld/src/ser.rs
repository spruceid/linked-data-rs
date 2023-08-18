use rdf_types::{Interpretation, Vocabulary};

mod graph;
mod id;
mod predicate;
mod subject;

pub use graph::*;
pub use id::*;
pub use predicate::*;
pub use subject::*;

/// Serialize a Linked-Data dataset.
pub trait SerializeLd<V: Vocabulary, I: Interpretation> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer<V, I>;
}

pub trait Serializer<V: Vocabulary, I: Interpretation> {
    type Ok;
    type Error;

    type GraphSerializer<'a>: GraphSerializer<V, I, Error = Self::Error>
    where
        Self: 'a;

    fn vocabulary(&self) -> &V;

    fn vocabulary_mut(&mut self) -> &mut V;

    fn interpretation(&self) -> &I;

    fn interpretation_mut(&mut self) -> &mut I;

    fn insert<L, T>(&mut self, name: Option<&L>, value: &T) -> Result<(), Self::Error>
    where
        L: ?Sized + LexicalRepresentation<V, I>,
        T: ?Sized + SerializeGraph<V, I>;

    fn end(self) -> Result<Self::Ok, Self::Error>;
}

impl<'s, V: Vocabulary, I: Interpretation, S: Serializer<V, I>> Serializer<V, I> for &'s mut S {
    type Ok = ();
    type Error = S::Error;

    type GraphSerializer<'a> = S::GraphSerializer<'a> where Self: 'a;

    fn vocabulary(&self) -> &V {
        S::vocabulary(self)
    }

    fn vocabulary_mut(&mut self) -> &mut V {
        S::vocabulary_mut(self)
    }

    fn interpretation(&self) -> &I {
        S::interpretation(self)
    }

    fn interpretation_mut(&mut self) -> &mut I {
        S::interpretation_mut(self)
    }

    fn insert<L, T>(&mut self, name: Option<&L>, value: &T) -> Result<(), Self::Error>
    where
        L: ?Sized + LexicalRepresentation<V, I>,
        T: ?Sized + SerializeGraph<V, I>,
    {
        S::insert(self, name, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}
