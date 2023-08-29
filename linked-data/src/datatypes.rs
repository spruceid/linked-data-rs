use iref::Iri;
use rdf_types::{
	literal::Type, Interpretation, IriVocabularyMut, LiteralVocabularyMut, Term, Vocabulary,
};

use crate::{
	CowRdfTerm, Interpret, LinkedDataPredicateObjects, LinkedDataSubject, PredicateObjectsVisitor,
	RdfLiteral, ResourceInterpretation,
};

macro_rules! datatype {
    ($($ty:ident : $iri:literal),*) => {
        $(
            impl<V: Vocabulary + IriVocabularyMut + LiteralVocabularyMut, I: Interpretation> Interpret<V, I> for $ty
            where
                V::Value: From<String>,
                V::Type: From<Type<V::Iri, V::LanguageTag>>,
            {
                fn interpret(
                    &self,
                    _interpretation: &mut I,
                    vocabulary: &mut V
                ) -> ResourceInterpretation<V, I> {
                    let ty = vocabulary.insert(Iri::new($iri).unwrap());
                    ResourceInterpretation::Uninterpreted(Some(CowRdfTerm::Owned(Term::Literal(RdfLiteral::Any(
                        self.to_string(),
                        Type::Any(ty).into(),
                    )))))
                }
            }

            impl<V: Vocabulary + IriVocabularyMut + LiteralVocabularyMut, I: Interpretation> LinkedDataSubject<V, I> for $ty
            where
                V::Value: From<String>,
                V::Type: From<Type<V::Iri, V::LanguageTag>>,
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
                V::Value: From<String>,
                V::Type: From<Type<V::Iri, V::LanguageTag>>,
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
	u8: "http://www.w3.org/2001/XMLSchema#unsignedByte",
	u16: "http://www.w3.org/2001/XMLSchema#unsignedShort",
	u32: "http://www.w3.org/2001/XMLSchema#unsignedInt",
	u64: "http://www.w3.org/2001/XMLSchema#unsignedLong",
	i8: "http://www.w3.org/2001/XMLSchema#byte",
	i16: "http://www.w3.org/2001/XMLSchema#short",
	i32: "http://www.w3.org/2001/XMLSchema#int",
	i64: "http://www.w3.org/2001/XMLSchema#long",
	String: "http://www.w3.org/2001/XMLSchema#string"
}
