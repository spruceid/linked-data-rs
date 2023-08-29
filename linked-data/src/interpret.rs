use educe::Educe;
use iref::{Iri, IriBuf};
use rdf_types::{
	BlankId, BlankIdBuf, BlankIdVocabularyMut, Id, Interpretation, IriVocabularyMut, Term,
	Vocabulary, interpretation::{ReverseIriInterpretation, ReverseBlankIdInterpretation}, ReverseLiteralInterpretation,
};
use std::fmt;

use crate::{CowRdfTerm, RdfLiteralValue};

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

	pub fn into_lexical_representation(
		self,
		vocabulary: &'a V,
		interpretation: &'a I
	) -> Option<CowRdfTerm<'a, V>>
	where
		V::Value: RdfLiteralValue<V>,
		I: ReverseIriInterpretation<Iri = V::Iri> + ReverseBlankIdInterpretation<BlankId = V::BlankId> + ReverseLiteralInterpretation<Literal = V::Literal>
	{
		match self {
			Self::Interpreted(r) => {
				if let Some(i) = interpretation.iris_of(r).next() {
					return Some(CowRdfTerm::Borrowed(Term::Id(Id::Iri(i))));
				}

				if let Some(l) = interpretation.literals_of(r).next() {
					let l = vocabulary.literal(l).unwrap();
					let term = match l.value().as_rdf_literal(vocabulary, l.type_()) {
						crate::CowRdfLiteral::Borrowed(l) => CowRdfTerm::Borrowed(Term::Literal(l)),
						crate::CowRdfLiteral::Owned(l) => CowRdfTerm::Owned(Term::Literal(l))
					};

					return Some(term)
				}

				if let Some(b) = interpretation.blank_ids_of(r).next() {
					return Some(CowRdfTerm::Borrowed(Term::Id(Id::Blank(b))));
				}

				None
			}
			Self::Uninterpreted(t) => t
		}
	}
}

/// Type that can have an interpretation bound to the given lifetime.
pub trait InterpretRef<'a, V: Vocabulary, I: Interpretation> {
	fn interpret_ref(
		&self,
		vocabulary: &mut V,
		interpretation: &mut I,
	) -> ResourceInterpretation<'a, V, I>;
}

/// Type that can have an interpretation.
pub trait Interpret<V: Vocabulary, I: Interpretation> {
	fn interpret(&self, vocabulary: &mut V, interpretation: &mut I)
		-> ResourceInterpretation<V, I>;

	fn lexical_representation<'a>(
		&'a self,
		vocabulary: &'a mut V,
		interpretation: &'a mut I
	) -> Option<CowRdfTerm<'a, V>>
	where
		V::Value: RdfLiteralValue<V>,
		I: ReverseIriInterpretation<Iri = V::Iri> + ReverseBlankIdInterpretation<BlankId = V::BlankId> + ReverseLiteralInterpretation<Literal = V::Literal>
	{
		self.interpret(vocabulary, interpretation).into_lexical_representation(vocabulary, interpretation)
	}
}

/// Anonymous lexical representation.
///
/// This type implements the `LexicalRepresentation` trait, producing a blank
/// node identifier.
pub struct Anonymous;

impl<V: Vocabulary, I: Interpretation> Interpret<V, I> for Anonymous {
	fn interpret(
		&self,
		_vocabulary: &mut V,
		_interpretation: &mut I,
	) -> ResourceInterpretation<V, I> {
		ResourceInterpretation::Uninterpreted(None)
	}
}

impl<V: Vocabulary + IriVocabularyMut, I: Interpretation> Interpret<V, I> for Iri {
	fn interpret(
		&self,
		vocabulary: &mut V,
		_interpretation: &mut I,
	) -> ResourceInterpretation<V, I> {
		ResourceInterpretation::Uninterpreted(Some(CowRdfTerm::Owned(Term::Id(Id::Iri(
			vocabulary.insert(self),
		)))))
	}
}

impl<V: Vocabulary + IriVocabularyMut, I: Interpretation> Interpret<V, I> for IriBuf {
	fn interpret(
		&self,
		vocabulary: &mut V,
		_interpretation: &mut I,
	) -> ResourceInterpretation<V, I> {
		ResourceInterpretation::Uninterpreted(Some(CowRdfTerm::Owned(Term::Id(Id::Iri(
			vocabulary.insert(self),
		)))))
	}
}

impl<V: Vocabulary + BlankIdVocabularyMut, I: Interpretation> Interpret<V, I> for BlankId {
	fn interpret(
		&self,
		vocabulary: &mut V,
		_interpretation: &mut I,
	) -> ResourceInterpretation<V, I> {
		ResourceInterpretation::Uninterpreted(Some(CowRdfTerm::Owned(Term::Id(Id::Blank(
			vocabulary.insert_blank_id(self),
		)))))
	}
}

impl<V: Vocabulary + BlankIdVocabularyMut, I: Interpretation> Interpret<V, I> for BlankIdBuf {
	fn interpret(
		&self,
		vocabulary: &mut V,
		_interpretation: &mut I,
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
		vocabulary: &mut V,
		interpretation: &mut I,
	) -> ResourceInterpretation<V, I> {
		match self {
			Self::Iri(i) => i.interpret(vocabulary, interpretation),
			Self::Blank(b) => b.interpret(vocabulary, interpretation),
		}
	}
}

impl<V: Vocabulary + IriVocabularyMut + BlankIdVocabularyMut, I: Interpretation> Interpret<V, I>
	for Id<IriBuf, BlankIdBuf>
{
	fn interpret(
		&self,
		vocabulary: &mut V,
		interpretation: &mut I,
	) -> ResourceInterpretation<V, I> {
		match self {
			Self::Iri(i) => i.interpret(vocabulary, interpretation),
			Self::Blank(b) => b.interpret(vocabulary, interpretation),
		}
	}
}

impl<V: Vocabulary, I: Interpretation, T: Interpret<V, I>> Interpret<V, I> for Option<T> {
	fn interpret(
		&self,
		vocabulary: &mut V,
		interpretation: &mut I,
	) -> ResourceInterpretation<V, I> {
		match self {
			Some(t) => t.interpret(vocabulary, interpretation),
			None => ResourceInterpretation::Uninterpreted(None),
		}
	}
}
