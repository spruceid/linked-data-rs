use rdf_types::{Interpretation, Vocabulary};

use crate::{LexicalRepresentation, SerializeGraph, SerializePredicate};

/// Serialize a Linked-Data node.
pub trait SerializeSubject<V: Vocabulary, I: Interpretation> {
    fn serialize_subject<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: SubjectSerializer<V, I>;
}

pub trait SubjectSerializer<V: Vocabulary, I: Interpretation> {
    type Ok;
    type Error;

    fn interpretation(&self) -> &I;

    fn interpretation_mut(&mut self) -> &mut I;

    fn vocabulary(&self) -> &V;

    fn vocabulary_mut(&mut self) -> &mut V;

    fn begin<L, T>(self, term: &L, properties: &T) -> Result<Self::Ok, Self::Error>
    where
        L: ?Sized + LexicalRepresentation<V, I>,
        T: ?Sized + SerializeSubjectProperties<V, I>;
}

/// Serialize a named Linked-Data node.
pub trait SerializeSubjectProperties<V: Vocabulary, I: Interpretation> {
    fn serialize_subject_properties<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: SubjectPropertiesSerializer<V, I>;
}

pub trait SubjectPropertiesSerializer<V: Vocabulary, I: Interpretation> {
    type Ok;
    type Error;

    fn vocabulary(&self) -> &V;

    fn vocabulary_mut(&mut self) -> &mut V;

    fn interpretation(&self) -> &I;

    fn interpretation_mut(&mut self) -> &mut I;

    fn insert<L, T>(&mut self, predicate: &L, value: &T) -> Result<(), Self::Error>
    where
        L: ?Sized + LexicalRepresentation<V, I>,
        T: ?Sized + SerializePredicate<V, I>;

    fn graph<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + SerializeGraph<V, I>;

    fn end(self) -> Result<Self::Ok, Self::Error>;
}

impl<'s, V: Vocabulary, I: Interpretation, S: SubjectPropertiesSerializer<V, I>>
    SubjectPropertiesSerializer<V, I> for &'s mut S
{
    type Ok = ();
    type Error = S::Error;

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

    fn insert<L, T>(&mut self, predicate: &L, value: &T) -> Result<(), Self::Error>
    where
        L: ?Sized + LexicalRepresentation<V, I>,
        T: ?Sized + SerializePredicate<V, I>,
    {
        S::insert(self, predicate, value)
    }

    fn graph<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + SerializeGraph<V, I>,
    {
        S::graph(self, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}
