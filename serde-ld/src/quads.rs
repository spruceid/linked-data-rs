use rdf_types::{
    BlankIdVocabulary, Generator, Id, IriVocabulary, LiteralVocabulary, Quad, Term, Vocabulary,
};

use crate::{
    GraphSerializer, LexicalRepresentation, PredicateSerializer, SerializeGraph, SerializeLd,
    Serializer, SubjectSerializer,
};

pub type RdfId<V> = Id<<V as IriVocabulary>::Iri, <V as BlankIdVocabulary>::BlankId>;
pub type RdfTerm<V> = Term<RdfId<V>, <V as LiteralVocabulary>::Literal>;
pub type RdfQuad<V> = Quad<RdfId<V>, <V as IriVocabulary>::Iri, RdfTerm<V>, RdfId<V>>;

pub fn to_quads_with<V: Vocabulary, I>(
    vocabulary: &mut V,
    interpretation: &mut I,
    generator: impl Generator<V>,
    value: &impl SerializeLd<V, I>,
) -> Result<Vec<RdfQuad<V>>, Error>
where
    V::BlankId: Clone,
    V::Iri: Clone,
    V::Literal: Clone,
{
    value.serialize(QuadSerializer {
        vocabulary,
        interpretation,
        generator,
        result: Vec::new(),
    })
}

pub fn to_quads(
    generator: impl Generator<()>,
    value: &impl SerializeLd,
) -> Result<Vec<Quad>, Error> {
    value.serialize(QuadSerializer {
        vocabulary: &mut (),
        interpretation: &mut (),
        generator,
        result: Vec::new(),
    })
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("invalid graph label")]
    Graph,

    #[error("invalid subject")]
    Subject,

    #[error("invalid predicate")]
    Predicate,
}

/// A simple serializer generating a list of `Quad`s.
pub struct QuadSerializer<'a, V: Vocabulary, I, G> {
    vocabulary: &'a mut V,
    interpretation: &'a mut I,
    generator: G,
    result: Vec<RdfQuad<V>>,
}

impl<'a, V: Vocabulary, I, G> Serializer<V, I> for QuadSerializer<'a, V, I, G>
where
    V::BlankId: Clone,
    V::Iri: Clone,
    V::Literal: Clone,
    G: Generator<V>,
{
    type Ok = Vec<RdfQuad<V>>;
    type Error = Error;

    fn insert_default<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + crate::SerializeGraph<V, I>,
    {
        let graph_serializer = QuadGraphSerializer {
            vocabulary: self.vocabulary,
            interpretation: self.interpretation,
            generator: &mut self.generator,
            result: &mut self.result,
            graph: None,
        };

        value.serialize_graph(graph_serializer)
    }

    fn insert<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + LexicalRepresentation<V, I> + crate::SerializeGraph<V, I>,
    {
        let graph = value
            .lexical_representation(self.interpretation, self.vocabulary, &mut self.generator)
            .into_id()
            .ok_or(Error::Graph)?;
        let graph_serializer = QuadGraphSerializer {
            vocabulary: self.vocabulary,
            interpretation: self.interpretation,
            generator: &mut self.generator,
            result: &mut self.result,
            graph: Some(&graph),
        };

        value.serialize_graph(graph_serializer)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self.result)
    }
}

pub struct QuadGraphSerializer<'a, V: Vocabulary, I, G> {
    vocabulary: &'a mut V,
    interpretation: &'a mut I,
    generator: &'a mut G,
    result: &'a mut Vec<RdfQuad<V>>,
    graph: Option<&'a RdfId<V>>,
}

impl<'a, V: Vocabulary, I, G> GraphSerializer<V, I> for QuadGraphSerializer<'a, V, I, G>
where
    V::BlankId: Clone,
    V::Iri: Clone,
    V::Literal: Clone,
    G: Generator<V>,
{
    type Ok = ();
    type Error = Error;

    fn insert<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + LexicalRepresentation<V, I> + crate::SerializeSubject<V, I>,
    {
        let term =
            value.lexical_representation(self.interpretation, self.vocabulary, self.generator);
        let properties_serializer = QuadPropertiesSerializer {
            vocabulary: self.vocabulary,
            interpretation: self.interpretation,
            generator: self.generator,
            result: self.result,
            graph: self.graph,
            subject: &term,
        };

        value.serialize_subject(properties_serializer)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}

pub struct QuadPropertiesSerializer<'a, V: Vocabulary, I, G> {
    vocabulary: &'a mut V,
    interpretation: &'a mut I,
    generator: &'a mut G,
    result: &'a mut Vec<RdfQuad<V>>,
    graph: Option<&'a RdfId<V>>,
    subject: &'a RdfTerm<V>,
}

impl<'a, V: Vocabulary, I, G> SubjectSerializer<V, I> for QuadPropertiesSerializer<'a, V, I, G>
where
    V::BlankId: Clone,
    V::Iri: Clone,
    V::Literal: Clone,
    G: Generator<V>,
{
    type Ok = ();
    type Error = Error;

    fn insert<L, T>(&mut self, predicate: &L, value: &T) -> Result<(), Self::Error>
    where
        L: ?Sized + LexicalRepresentation<V, I>,
        T: ?Sized + crate::SerializePredicate<V, I>,
    {
        match self.subject {
            Term::Id(subject) => {
                match predicate.lexical_representation(
                    self.interpretation,
                    self.vocabulary,
                    self.generator,
                ) {
                    Term::Id(Id::Iri(iri)) => {
                        let objects_serializer = ObjectsSerializer {
                            vocabulary: self.vocabulary,
                            interpretation: self.interpretation,
                            generator: self.generator,
                            result: self.result,
                            graph: self.graph,
                            subject,
                            predicate: iri,
                        };

                        value.serialize_predicate(objects_serializer)
                    }
                    _ => Err(Error::Predicate),
                }
            }
            _ => Err(Error::Subject),
        }
    }

    fn graph<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + LexicalRepresentation<V, I> + SerializeGraph<V, I>,
    {
        let graph = value
            .lexical_representation(self.interpretation, self.vocabulary, &mut self.generator)
            .into_id()
            .ok_or(Error::Graph)?;

        let graph_serializer = QuadGraphSerializer {
            vocabulary: self.vocabulary,
            interpretation: self.interpretation,
            generator: &mut self.generator,
            result: self.result,
            graph: Some(&graph),
        };

        value.serialize_graph(graph_serializer)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}

pub struct ObjectsSerializer<'a, V: Vocabulary, I, G> {
    vocabulary: &'a mut V,
    interpretation: &'a mut I,
    generator: &'a mut G,
    result: &'a mut Vec<RdfQuad<V>>,
    graph: Option<&'a RdfId<V>>,
    subject: &'a RdfId<V>,
    predicate: V::Iri,
}

impl<'a, V: Vocabulary, I, G> PredicateSerializer<V, I> for ObjectsSerializer<'a, V, I, G>
where
    V::BlankId: Clone,
    V::Iri: Clone,
    V::Literal: Clone,
    G: Generator<V>,
{
    type Ok = ();
    type Error = Error;

    fn insert<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + LexicalRepresentation<V, I> + crate::SerializeSubject<V, I>,
    {
        let term =
            value.lexical_representation(self.interpretation, self.vocabulary, self.generator);
        let subject_serializer = QuadPropertiesSerializer {
            vocabulary: self.vocabulary,
            interpretation: self.interpretation,
            generator: self.generator,
            result: self.result,
            graph: self.graph,
            subject: &term,
        };

        value.serialize_subject(subject_serializer)?;
        self.result.push(Quad(
            self.subject.clone(),
            self.predicate.clone(),
            term,
            self.graph.cloned(),
        ));
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(())
    }
}
