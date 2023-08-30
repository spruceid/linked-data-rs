use rdf_types::{Interpretation, IriVocabularyMut, LiteralVocabularyMut, Term, Vocabulary};

use crate::{
	CowRdfTerm, Interpret, LinkedDataPredicateObjects, LinkedDataSubject, PredicateObjectsVisitor,
	RdfLiteral, RdfLiteralRef, ResourceInterpretation,
};

macro_rules! datatype {
	($($ty:ty : $variant:ident),*) => {
		$(
			impl<V: Vocabulary + IriVocabularyMut + LiteralVocabularyMut, I: Interpretation> Interpret<V, I> for $ty
			where
				V::Value: From<String>
			{
				fn interpret(
					&self,
					_vocabulary: &mut V,
					_interpretation: &mut I,
				) -> ResourceInterpretation<V, I> {
					ResourceInterpretation::Uninterpreted(Some(CowRdfTerm::Owned(Term::Literal(RdfLiteral::Xsd(
						xsd_types::Value::$variant(self.clone())
					)))))
				}
			}

			impl<V: Vocabulary + IriVocabularyMut + LiteralVocabularyMut, I: Interpretation> LinkedDataSubject<V, I> for $ty
			where
				V::Value: From<String>
			{
				fn visit_subject<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
				where
					S: crate::SubjectVisitor<V, I>
				{
					visitor.end()
				}
			}

			impl<V: Vocabulary + IriVocabularyMut + LiteralVocabularyMut, I: Interpretation> LinkedDataPredicateObjects<V, I> for $ty
			where
				V::Value: From<String>
			{
				fn visit_objects<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
				where
					S: PredicateObjectsVisitor<V, I>,
				{
					visitor.object(self)?;
					visitor.end()
				}
			}
		)*
	};
}

macro_rules! unsized_datatype {
	($($ty:ty : $variant:ident),*) => {
		$(
			impl<V: Vocabulary + IriVocabularyMut + LiteralVocabularyMut, I: Interpretation> Interpret<V, I> for $ty
			where
				V::Value: From<String>
			{
				fn interpret(
					&self,
					_vocabulary: &mut V,
					_interpretation: &mut I,
				) -> ResourceInterpretation<V, I> {
					ResourceInterpretation::Uninterpreted(Some(CowRdfTerm::Borrowed(Term::Literal(RdfLiteralRef::Xsd(
						xsd_types::ValueRef::$variant(self)
					)))))
				}
			}

			impl<V: Vocabulary + IriVocabularyMut + LiteralVocabularyMut, I: Interpretation> LinkedDataSubject<V, I> for $ty
			where
				V::Value: From<String>
			{
				fn visit_subject<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
				where
					S: crate::SubjectVisitor<V, I>
				{
					visitor.end()
				}
			}

			impl<V: Vocabulary + IriVocabularyMut + LiteralVocabularyMut, I: Interpretation> LinkedDataPredicateObjects<V, I> for $ty
			where
				V::Value: From<String>
			{
				fn visit_objects<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
				where
					S: PredicateObjectsVisitor<V, I>,
				{
					visitor.object(self)?;
					visitor.end()
				}
			}
		)*
	};
}

datatype! {
	u8: UnsignedByte,
	u16: UnsignedShort,
	u32: UnsignedInt,
	u64: UnsignedLong,
	i8: Byte,
	i16: Short,
	i32: Int,
	i64: Long,
	String: String,
	iref::UriBuf: AnyUri,
	xsd_types::DateTime: DateTime
}

unsized_datatype! {
	str: String
}
