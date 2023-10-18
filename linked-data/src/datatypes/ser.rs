use rdf_types::{Id, Interpretation, IriVocabularyMut, LiteralVocabularyMut, Term, Vocabulary};

use crate::{
	CowRdfTerm, LinkedDataPredicateObjects, LinkedDataResource, LinkedDataSubject,
	PredicateObjectsVisitor, RdfLiteral, RdfLiteralRef, ResourceInterpretation,
};

macro_rules! datatype {
	($($ty:ty : $variant:ident),*) => {
		$(
			impl<V: Vocabulary + IriVocabularyMut + LiteralVocabularyMut, I: Interpretation> LinkedDataResource<I, V> for $ty
			where
				V::Value: From<String>
			{
				fn interpretation(
					&self,
					_vocabulary: &mut V,
					_interpretation: &mut I,
				) -> ResourceInterpretation<I, V> {
					ResourceInterpretation::Uninterpreted(Some(CowRdfTerm::Owned(Term::Literal(RdfLiteral::Xsd(
						xsd_types::Value::$variant(self.clone())
					)))))
				}
			}

			impl<V: Vocabulary + IriVocabularyMut + LiteralVocabularyMut, I: Interpretation> LinkedDataSubject<I, V> for $ty
			where
				V::Value: From<String>
			{
				fn visit_subject<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
				where
					S: crate::SubjectVisitor<I, V>
				{
					visitor.end()
				}
			}

			impl<V: Vocabulary + IriVocabularyMut + LiteralVocabularyMut, I: Interpretation> LinkedDataPredicateObjects<I, V> for $ty
			where
				V::Value: From<String>
			{
				fn visit_objects<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
				where
					S: PredicateObjectsVisitor<I, V>,
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
			impl<V: Vocabulary + IriVocabularyMut + LiteralVocabularyMut, I: Interpretation> LinkedDataResource<I, V> for $ty
			where
				V::Value: From<String>
			{
				fn interpretation(
					&self,
					_vocabulary: &mut V,
					_interpretation: &mut I,
				) -> ResourceInterpretation<I, V> {
					ResourceInterpretation::Uninterpreted(Some(CowRdfTerm::Borrowed(Term::Literal(RdfLiteralRef::Xsd(
						xsd_types::ValueRef::$variant(self)
					)))))
				}
			}

			impl<V: Vocabulary + IriVocabularyMut + LiteralVocabularyMut, I: Interpretation> LinkedDataSubject<I, V> for $ty
			where
				V::Value: From<String>
			{
				fn visit_subject<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
				where
					S: crate::SubjectVisitor<I, V>
				{
					visitor.end()
				}
			}

			impl<V: Vocabulary + IriVocabularyMut + LiteralVocabularyMut, I: Interpretation> LinkedDataPredicateObjects<I, V> for $ty
			where
				V::Value: From<String>
			{
				fn visit_objects<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
				where
					S: PredicateObjectsVisitor<I, V>,
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
	xsd_types::DateTime: DateTime
}

unsized_datatype! {
	str: String
}

impl<V: Vocabulary + IriVocabularyMut + LiteralVocabularyMut, I: Interpretation>
	LinkedDataResource<I, V> for xsd_types::AnyUriBuf
where
	V::Value: From<String>,
{
	fn interpretation(
		&self,
		_vocabulary: &mut V,
		_interpretation: &mut I,
	) -> ResourceInterpretation<I, V> {
		ResourceInterpretation::Uninterpreted(Some(CowRdfTerm::Owned(Term::Literal(
			RdfLiteral::Xsd(xsd_types::Value::AnyUri(self.clone())),
		))))
	}

	fn reference_interpretation(
		&self,
		vocabulary: &mut V,
		_interpretation: &mut I,
	) -> ResourceInterpretation<I, V> {
		ResourceInterpretation::Uninterpreted(Some(CowRdfTerm::Owned(Term::Id(Id::Iri(
			vocabulary.insert(self.as_iri()),
		)))))
	}
}

impl<V: Vocabulary + IriVocabularyMut + LiteralVocabularyMut, I: Interpretation>
	LinkedDataSubject<I, V> for xsd_types::AnyUriBuf
where
	V::Value: From<String>,
{
	fn visit_subject<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: crate::SubjectVisitor<I, V>,
	{
		visitor.end()
	}
}

impl<V: Vocabulary + IriVocabularyMut + LiteralVocabularyMut, I: Interpretation>
	LinkedDataPredicateObjects<I, V> for xsd_types::AnyUriBuf
where
	V::Value: From<String>,
{
	fn visit_objects<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<I, V>,
	{
		visitor.object(self)?;
		visitor.end()
	}
}
