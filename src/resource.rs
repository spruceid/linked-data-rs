use educe::Educe;
use iref::{Iri, IriBuf};
use rdf_types::{
	interpretation::ReverseTermInterpretation,
	vocabulary::{BlankIdVocabularyMut, IriVocabularyMut},
	BlankId, BlankIdBuf, Id, Interpretation, Term, Vocabulary,
};
use std::fmt;

use crate::{AsRdfLiteral, CowRdfTerm};

/// Resource interpretation.
#[derive(Educe)]
#[educe(Debug(bound = "I::Resource: fmt::Debug, V::Iri: fmt::Debug, V::BlankId: fmt::Debug"))]
pub enum ResourceInterpretation<'a, I: Interpretation, V: Vocabulary> {
	/// Interpreted resource.
	Interpreted(&'a I::Resource),

	/// Uninterpreted resource with the given optional lexical representation.
	///
	/// It can be used to give an actual interpretation to the resource, bound
	/// to its lexical representation.
	Uninterpreted(Option<CowRdfTerm<'a, V>>),
}

impl<'a, I: Interpretation, V: Vocabulary> ResourceInterpretation<'a, I, V> {
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
		interpretation: &'a I,
	) -> Option<CowRdfTerm<'a, V>>
	where
		I: ReverseTermInterpretation<Iri = V::Iri, BlankId = V::BlankId, Literal = V::Literal>,
	{
		match self {
			Self::Interpreted(r) => {
				if let Some(i) = interpretation.iris_of(r).next() {
					return Some(CowRdfTerm::Borrowed(Term::Id(Id::Iri(i))));
				}

				if let Some(l) = interpretation.literals_of(r).next() {
					let l = vocabulary.literal(l).unwrap();
					let term = match l.value.as_rdf_literal(vocabulary, l.type_) {
						crate::CowRdfLiteral::Borrowed(l) => CowRdfTerm::Borrowed(Term::Literal(l)),
						crate::CowRdfLiteral::Owned(l) => CowRdfTerm::Owned(Term::Literal(l)),
					};

					return Some(term);
				}

				if let Some(b) = interpretation.blank_ids_of(r).next() {
					return Some(CowRdfTerm::Borrowed(Term::Id(Id::Blank(b))));
				}

				None
			}
			Self::Uninterpreted(t) => t,
		}
	}
}

/// Type that can have an interpretation bound to the given lifetime.
pub trait LinkedDataResourceRef<'a, I: Interpretation = (), V: Vocabulary = ()> {
	fn interpretation_ref(
		&self,
		vocabulary: &mut V,
		interpretation: &mut I,
	) -> ResourceInterpretation<'a, I, V>;
}

/// Type that can have an interpretation.
pub trait LinkedDataResource<I: Interpretation = (), V: Vocabulary = ()> {
	fn interpretation(
		&self,
		vocabulary: &mut V,
		interpretation: &mut I,
	) -> ResourceInterpretation<I, V>;

	fn lexical_representation<'a>(
		&'a self,
		vocabulary: &'a mut V,
		interpretation: &'a mut I,
	) -> Option<CowRdfTerm<'a, V>>
	where
		I: ReverseTermInterpretation<Iri = V::Iri, BlankId = V::BlankId, Literal = V::Literal>,
	{
		self.interpretation(vocabulary, interpretation)
			.into_lexical_representation(vocabulary, interpretation)
	}

	fn reference_interpretation(
		&self,
		vocabulary: &mut V,
		interpretation: &mut I,
	) -> ResourceInterpretation<I, V> {
		self.interpretation(vocabulary, interpretation)
	}
}

impl<'a, I: Interpretation, V: Vocabulary, T: ?Sized + LinkedDataResource<I, V>>
	LinkedDataResource<I, V> for &'a T
{
	fn interpretation(
		&self,
		vocabulary: &mut V,
		interpretation: &mut I,
	) -> ResourceInterpretation<I, V> {
		T::interpretation(self, vocabulary, interpretation)
	}
}

impl<I: Interpretation, V: Vocabulary, T: ?Sized + LinkedDataResource<I, V>>
	LinkedDataResource<I, V> for Box<T>
{
	fn interpretation(
		&self,
		vocabulary: &mut V,
		interpretation: &mut I,
	) -> ResourceInterpretation<I, V> {
		T::interpretation(self, vocabulary, interpretation)
	}
}

impl<I: Interpretation, V: Vocabulary> LinkedDataResource<I, V> for () {
	fn interpretation(
		&self,
		_vocabulary: &mut V,
		_interpretation: &mut I,
	) -> ResourceInterpretation<I, V> {
		ResourceInterpretation::Uninterpreted(None)
	}
}

/// Anonymous lexical representation.
///
/// This type implements the `LexicalRepresentation` trait, producing a blank
/// node identifier.
pub struct Anonymous;

impl<I: Interpretation, V: Vocabulary> LinkedDataResource<I, V> for Anonymous {
	fn interpretation(
		&self,
		_vocabulary: &mut V,
		_interpretation: &mut I,
	) -> ResourceInterpretation<I, V> {
		ResourceInterpretation::Uninterpreted(None)
	}
}

impl<V: Vocabulary + IriVocabularyMut, I: Interpretation> LinkedDataResource<I, V> for Iri {
	fn interpretation(
		&self,
		vocabulary: &mut V,
		_interpretation: &mut I,
	) -> ResourceInterpretation<I, V> {
		ResourceInterpretation::Uninterpreted(Some(CowRdfTerm::Owned(Term::Id(Id::Iri(
			vocabulary.insert(self),
		)))))
	}
}

impl<V: Vocabulary + IriVocabularyMut, I: Interpretation> LinkedDataResource<I, V> for IriBuf {
	fn interpretation(
		&self,
		vocabulary: &mut V,
		_interpretation: &mut I,
	) -> ResourceInterpretation<I, V> {
		ResourceInterpretation::Uninterpreted(Some(CowRdfTerm::Owned(Term::Id(Id::Iri(
			vocabulary.insert(self),
		)))))
	}
}

impl<V: Vocabulary + BlankIdVocabularyMut, I: Interpretation> LinkedDataResource<I, V> for BlankId {
	fn interpretation(
		&self,
		vocabulary: &mut V,
		_interpretation: &mut I,
	) -> ResourceInterpretation<I, V> {
		ResourceInterpretation::Uninterpreted(Some(CowRdfTerm::Owned(Term::Id(Id::Blank(
			vocabulary.insert_blank_id(self),
		)))))
	}
}

impl<V: Vocabulary + BlankIdVocabularyMut, I: Interpretation> LinkedDataResource<I, V>
	for BlankIdBuf
{
	fn interpretation(
		&self,
		vocabulary: &mut V,
		_interpretation: &mut I,
	) -> ResourceInterpretation<I, V> {
		ResourceInterpretation::Uninterpreted(Some(CowRdfTerm::Owned(Term::Id(Id::Blank(
			vocabulary.insert_blank_id(self),
		)))))
	}
}

impl<
		V: Vocabulary,
		I: Interpretation,
		T: LinkedDataResource<I, V>,
		B: LinkedDataResource<I, V>,
	> LinkedDataResource<I, V> for Id<T, B>
{
	fn interpretation(
		&self,
		vocabulary: &mut V,
		interpretation: &mut I,
	) -> ResourceInterpretation<I, V> {
		match self {
			Self::Iri(i) => i.reference_interpretation(vocabulary, interpretation),
			Self::Blank(b) => b.reference_interpretation(vocabulary, interpretation),
		}
	}
}

impl<
		V: Vocabulary,
		I: Interpretation,
		T: LinkedDataResource<I, V>,
		B: LinkedDataResource<I, V>,
		L: LinkedDataResource<I, V>,
	> LinkedDataResource<I, V> for Term<Id<T, B>, L>
{
	fn interpretation(
		&self,
		vocabulary: &mut V,
		interpretation: &mut I,
	) -> ResourceInterpretation<I, V> {
		match self {
			Self::Id(id) => id.reference_interpretation(vocabulary, interpretation),
			Self::Literal(l) => l.interpretation(vocabulary, interpretation),
		}
	}
}

impl<V: Vocabulary, I: Interpretation> LinkedDataResource<I, V> for rdf_types::Literal<V::Iri> {
	fn interpretation(
		&self,
		_vocabulary: &mut V,
		_interpretation: &mut I,
	) -> ResourceInterpretation<I, V> {
		ResourceInterpretation::Uninterpreted(Some(CowRdfTerm::Borrowed(Term::Literal(
			crate::RdfLiteralRef::Any(self.value.as_ref(), self.type_.as_ref()),
		))))
	}
}

impl<I: Interpretation, V: Vocabulary, T: LinkedDataResource<I, V>> LinkedDataResource<I, V>
	for Option<T>
{
	fn interpretation(
		&self,
		vocabulary: &mut V,
		interpretation: &mut I,
	) -> ResourceInterpretation<I, V> {
		match self {
			Some(t) => t.interpretation(vocabulary, interpretation),
			None => ResourceInterpretation::Uninterpreted(None),
		}
	}
}
