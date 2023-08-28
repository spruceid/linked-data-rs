use iref::{Iri, IriBuf};
use rdf_types::{
	BlankId, BlankIdBuf, BlankIdVocabularyMut, Id, IriVocabularyMut, Term, Vocabulary,
};

use crate::RdfTerm;

/// Type that can have a lexical representation.
pub trait LexicalRepresentation<V: Vocabulary, I> {
	fn lexical_representation(
		&self,
		interpretation: &mut I,
		vocabulary: &mut V,
	) -> Option<RdfTerm<V>>;
}

/// Anonymous lexical representation.
///
/// This type implements the `LexicalRepresentation` trait, producing a blank
/// node identifier.
pub struct Anonymous;

impl<V: Vocabulary, I> LexicalRepresentation<V, I> for Anonymous {
	fn lexical_representation(
		&self,
		_interpretation: &mut I,
		_vocabulary: &mut V,
	) -> Option<RdfTerm<V>> {
		None
	}
}

impl<V: Vocabulary + IriVocabularyMut, I> LexicalRepresentation<V, I> for Iri {
	fn lexical_representation(
		&self,
		_interpretation: &mut I,
		vocabulary: &mut V,
	) -> Option<RdfTerm<V>> {
		Some(Term::Id(Id::Iri(vocabulary.insert(self))))
	}
}

impl<V: Vocabulary + IriVocabularyMut, I> LexicalRepresentation<V, I> for IriBuf {
	fn lexical_representation(
		&self,
		_interpretation: &mut I,
		vocabulary: &mut V,
	) -> Option<RdfTerm<V>> {
		Some(Term::Id(Id::Iri(vocabulary.insert(self))))
	}
}

impl<V: Vocabulary + BlankIdVocabularyMut, I> LexicalRepresentation<V, I> for BlankId {
	fn lexical_representation(
		&self,
		_interpretation: &mut I,
		vocabulary: &mut V,
	) -> Option<RdfTerm<V>> {
		Some(Term::Id(Id::Blank(vocabulary.insert_blank_id(self))))
	}
}

impl<V: Vocabulary + BlankIdVocabularyMut, I> LexicalRepresentation<V, I> for BlankIdBuf {
	fn lexical_representation(
		&self,
		_interpretation: &mut I,
		vocabulary: &mut V,
	) -> Option<RdfTerm<V>> {
		Some(Term::Id(Id::Blank(vocabulary.insert_blank_id(self))))
	}
}

impl<'a, V: Vocabulary + IriVocabularyMut + BlankIdVocabularyMut, I> LexicalRepresentation<V, I>
	for Id<&'a Iri, &'a BlankId>
{
	fn lexical_representation(
		&self,
		interpretation: &mut I,
		vocabulary: &mut V,
	) -> Option<RdfTerm<V>> {
		match self {
			Self::Iri(i) => i.lexical_representation(interpretation, vocabulary),
			Self::Blank(b) => b.lexical_representation(interpretation, vocabulary),
		}
	}
}

impl<V: Vocabulary + IriVocabularyMut + BlankIdVocabularyMut, I> LexicalRepresentation<V, I>
	for Id<IriBuf, BlankIdBuf>
{
	fn lexical_representation(
		&self,
		interpretation: &mut I,
		vocabulary: &mut V,
	) -> Option<RdfTerm<V>> {
		match self {
			Self::Iri(i) => i.lexical_representation(interpretation, vocabulary),
			Self::Blank(b) => b.lexical_representation(interpretation, vocabulary),
		}
	}
}

impl<V: Vocabulary, I, T: LexicalRepresentation<V, I>> LexicalRepresentation<V, I> for Option<T> {
	fn lexical_representation(
		&self,
		interpretation: &mut I,
		vocabulary: &mut V,
	) -> Option<RdfTerm<V>> {
		self.as_ref()
			.and_then(|t| t.lexical_representation(interpretation, vocabulary))
	}
}
