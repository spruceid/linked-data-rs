use educe::Educe;
use iref::{Iri, IriBuf};
use rdf_types::{
	BlankId, BlankIdBuf, BlankIdVocabularyMut, Id, Interpretation, IriVocabularyMut, Term,
	Vocabulary,
};
use std::fmt;

use crate::CowRdfTerm;

/// Resource interpretation.
#[derive(Educe)]
#[educe(Debug(
	bound = "I::Resource: fmt::Debug, V::Iri: fmt::Debug, V::BlankId: fmt::Debug, V::LanguageTag: fmt::Debug"
))]
pub enum ResourceInterpretation<'a, V: Vocabulary, I: Interpretation> {
	/// Interpreted resource.
	Interpreted(&'a I::Resource),

	/// Uninterpreted resource with the given optional lexical representation.
	///
	/// It can be used to give an actual interpretation to the resource, bound
	/// to its lexical representation.
	Uninterpreted(Option<CowRdfTerm<'a, V>>),
}

impl<'a, V: Vocabulary, I: Interpretation> ResourceInterpretation<'a, V, I> {
	pub fn as_interpreted(&self) -> Option<&'a I::Resource> {
		match self {
			Self::Interpreted(i) => Some(i),
			_ => None,
		}
	}

	pub fn as_uninterpreted(&self) -> Option<&CowRdfTerm<'a, V>> {
		match self {
			Self::Uninterpreted(t) => t.as_ref(),
			_ => None,
		}
	}
}

/// Type that can have an interpretation bound to the given lifetime.
pub trait InterpretRef<'a, V: Vocabulary, I: Interpretation> {
	fn interpret_ref(
		&self,
		interpretation: &mut I,
		vocabulary: &mut V,
	) -> ResourceInterpretation<'a, V, I>;
}

/// Type that can have an interpretation.
pub trait Interpret<V: Vocabulary, I: Interpretation> {
	fn interpret(&self, interpretation: &mut I, vocabulary: &mut V)
		-> ResourceInterpretation<V, I>;
}

/// Anonymous lexical representation.
///
/// This type implements the `LexicalRepresentation` trait, producing a blank
/// node identifier.
pub struct Anonymous;

impl<V: Vocabulary, I: Interpretation> Interpret<V, I> for Anonymous {
	fn interpret(
		&self,
		_interpretation: &mut I,
		_vocabulary: &mut V,
	) -> ResourceInterpretation<V, I> {
		ResourceInterpretation::Uninterpreted(None)
	}
}

impl<V: Vocabulary + IriVocabularyMut, I: Interpretation> Interpret<V, I> for Iri {
	fn interpret(
		&self,
		_interpretation: &mut I,
		vocabulary: &mut V,
	) -> ResourceInterpretation<V, I> {
		ResourceInterpretation::Uninterpreted(Some(CowRdfTerm::Owned(Term::Id(Id::Iri(
			vocabulary.insert(self),
		)))))
	}
}

impl<V: Vocabulary + IriVocabularyMut, I: Interpretation> Interpret<V, I> for IriBuf {
	fn interpret(
		&self,
		_interpretation: &mut I,
		vocabulary: &mut V,
	) -> ResourceInterpretation<V, I> {
		ResourceInterpretation::Uninterpreted(Some(CowRdfTerm::Owned(Term::Id(Id::Iri(
			vocabulary.insert(self),
		)))))
	}
}

impl<V: Vocabulary + BlankIdVocabularyMut, I: Interpretation> Interpret<V, I> for BlankId {
	fn interpret(
		&self,
		_interpretation: &mut I,
		vocabulary: &mut V,
	) -> ResourceInterpretation<V, I> {
		ResourceInterpretation::Uninterpreted(Some(CowRdfTerm::Owned(Term::Id(Id::Blank(
			vocabulary.insert_blank_id(self),
		)))))
	}
}

impl<V: Vocabulary + BlankIdVocabularyMut, I: Interpretation> Interpret<V, I> for BlankIdBuf {
	fn interpret(
		&self,
		_interpretation: &mut I,
		vocabulary: &mut V,
	) -> ResourceInterpretation<V, I> {
		ResourceInterpretation::Uninterpreted(Some(CowRdfTerm::Owned(Term::Id(Id::Blank(
			vocabulary.insert_blank_id(self),
		)))))
	}
}

impl<'a, V: Vocabulary + IriVocabularyMut + BlankIdVocabularyMut, I: Interpretation> Interpret<V, I>
	for Id<&'a Iri, &'a BlankId>
{
	fn interpret(
		&self,
		interpretation: &mut I,
		vocabulary: &mut V,
	) -> ResourceInterpretation<V, I> {
		match self {
			Self::Iri(i) => i.interpret(interpretation, vocabulary),
			Self::Blank(b) => b.interpret(interpretation, vocabulary),
		}
	}
}

impl<V: Vocabulary + IriVocabularyMut + BlankIdVocabularyMut, I: Interpretation> Interpret<V, I>
	for Id<IriBuf, BlankIdBuf>
{
	fn interpret(
		&self,
		interpretation: &mut I,
		vocabulary: &mut V,
	) -> ResourceInterpretation<V, I> {
		match self {
			Self::Iri(i) => i.interpret(interpretation, vocabulary),
			Self::Blank(b) => b.interpret(interpretation, vocabulary),
		}
	}
}

impl<V: Vocabulary, I: Interpretation, T: Interpret<V, I>> Interpret<V, I> for Option<T> {
	fn interpret(
		&self,
		interpretation: &mut I,
		vocabulary: &mut V,
	) -> ResourceInterpretation<V, I> {
		match self {
			Some(t) => t.interpret(interpretation, vocabulary),
			None => ResourceInterpretation::Uninterpreted(None),
		}
	}
}
