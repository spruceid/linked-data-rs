use rdf_types::{Interpretation, Vocabulary};

use crate::{LexicalRepresentation, SerializeSubject};

/// Serialize a Linked-Data graph.
pub trait SerializeGraph<V: Vocabulary, I: Interpretation> {
    fn serialize_graph<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: GraphSerializer<V, I>;
}

pub trait GraphSerializer<V: Vocabulary, I: Interpretation> {
    type Ok;
    type Error;

    fn vocabulary(&self) -> &V;

    fn vocabulary_mut(&mut self) -> &mut V;

    fn interpretation(&self) -> &I;

    fn interpretation_mut(&mut self) -> &mut I;

    fn insert<L, T>(&mut self, subject: &L, value: &T) -> Result<(), Self::Error>
    where
        L: ?Sized + LexicalRepresentation<V, I>,
        T: ?Sized + SerializeSubject<V, I>;

    fn end(self) -> Result<Self::Ok, Self::Error>;
}
