use iref::{Iri, IriBuf};
use rdf_types::{
    BlankId, BlankIdBuf, BlankIdVocabularyMut, Generator, Id, Interpretation, IriVocabularyMut,
    Term, Vocabulary,
};

/// Type that can have a lexical representation.
pub trait LexicalRepresentation<V: Vocabulary, I: Interpretation> {
    fn lexical_representation(
        &self,
        interpretation: &mut I,
        vocabulary: &mut V,
        generator: &mut impl Generator<V>,
    ) -> Term<Id<V::Iri, V::BlankId>, V::Literal>;
}

/// Anonymous lexical representation.
///
/// This type implements the `LexicalRepresentation` trait, producing a blank
/// node identifier.
pub struct Anonymous;

impl Anonymous {
    pub fn lexical_representation<V: Vocabulary>(
        &self,
        vocabulary: &mut V,
        generator: &mut impl Generator<V>,
    ) -> Term<Id<V::Iri, V::BlankId>, V::Literal> {
        Term::Id(generator.next(vocabulary))
    }
}

impl<V: Vocabulary, I: Interpretation> LexicalRepresentation<V, I> for Anonymous {
    fn lexical_representation(
        &self,
        _interpretation: &mut I,
        vocabulary: &mut V,
        generator: &mut impl Generator<V>,
    ) -> Term<Id<V::Iri, V::BlankId>, V::Literal> {
        self.lexical_representation(vocabulary, generator)
    }
}

impl<V: Vocabulary + IriVocabularyMut, I: Interpretation> LexicalRepresentation<V, I> for Iri {
    fn lexical_representation(
        &self,
        _interpretation: &mut I,
        vocabulary: &mut V,
        _generator: &mut impl Generator<V>,
    ) -> Term<Id<V::Iri, V::BlankId>, V::Literal> {
        Term::Id(Id::Iri(vocabulary.insert(self)))
    }
}

impl<V: Vocabulary + IriVocabularyMut, I: Interpretation> LexicalRepresentation<V, I> for IriBuf {
    fn lexical_representation(
        &self,
        _interpretation: &mut I,
        vocabulary: &mut V,
        _generator: &mut impl Generator<V>,
    ) -> Term<Id<V::Iri, V::BlankId>, V::Literal> {
        Term::Id(Id::Iri(vocabulary.insert(self)))
    }
}

impl<V: Vocabulary + BlankIdVocabularyMut, I: Interpretation> LexicalRepresentation<V, I>
    for BlankId
{
    fn lexical_representation(
        &self,
        _interpretation: &mut I,
        vocabulary: &mut V,
        _generator: &mut impl Generator<V>,
    ) -> Term<Id<V::Iri, V::BlankId>, V::Literal> {
        Term::Id(Id::Blank(vocabulary.insert_blank_id(self)))
    }
}

impl<V: Vocabulary + BlankIdVocabularyMut, I: Interpretation> LexicalRepresentation<V, I>
    for BlankIdBuf
{
    fn lexical_representation(
        &self,
        _interpretation: &mut I,
        vocabulary: &mut V,
        _generator: &mut impl Generator<V>,
    ) -> Term<Id<V::Iri, V::BlankId>, V::Literal> {
        Term::Id(Id::Blank(vocabulary.insert_blank_id(self)))
    }
}

impl<'a, V: Vocabulary + IriVocabularyMut + BlankIdVocabularyMut, I: Interpretation>
    LexicalRepresentation<V, I> for Id<&'a Iri, &'a BlankId>
{
    fn lexical_representation(
        &self,
        interpretation: &mut I,
        vocabulary: &mut V,
        generator: &mut impl Generator<V>,
    ) -> Term<Id<V::Iri, V::BlankId>, V::Literal> {
        match self {
            Self::Iri(i) => i.lexical_representation(interpretation, vocabulary, generator),
            Self::Blank(b) => b.lexical_representation(interpretation, vocabulary, generator),
        }
    }
}

impl<V: Vocabulary + IriVocabularyMut + BlankIdVocabularyMut, I: Interpretation>
    LexicalRepresentation<V, I> for Id<IriBuf, BlankIdBuf>
{
    fn lexical_representation(
        &self,
        interpretation: &mut I,
        vocabulary: &mut V,
        generator: &mut impl Generator<V>,
    ) -> Term<Id<V::Iri, V::BlankId>, V::Literal> {
        match self {
            Self::Iri(i) => i.lexical_representation(interpretation, vocabulary, generator),
            Self::Blank(b) => b.lexical_representation(interpretation, vocabulary, generator),
        }
    }
}

impl<V: Vocabulary, I: Interpretation, T: LexicalRepresentation<V, I>> LexicalRepresentation<V, I>
    for Option<T>
{
    fn lexical_representation(
        &self,
        interpretation: &mut I,
        vocabulary: &mut V,
        generator: &mut impl Generator<V>,
    ) -> Term<Id<V::Iri, V::BlankId>, V::Literal> {
        match self {
            Some(t) => t.lexical_representation(interpretation, vocabulary, generator),
            None => Anonymous.lexical_representation(vocabulary, generator),
        }
    }
}
