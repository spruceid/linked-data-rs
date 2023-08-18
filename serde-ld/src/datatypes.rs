use iref::Iri;
use rdf_types::{literal::Type, Interpretation, Literal, Term, VocabularyMut};

use crate::{PredicateSerializer, SerializePredicate};

impl<V: VocabularyMut, I: Interpretation> SerializePredicate<V, I> for String
where
    V::Value: From<String>,
    V::Type: From<Type<V::Iri, V::LanguageTag>>,
{
    fn serialize_predicate<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
    where
        S: PredicateSerializer<V, I>,
    {
        let xsd_string = serializer
            .vocabulary_mut()
            .insert(Iri::new("http://www.w3.org/2001/XMLSchema#string").unwrap());
        let literal = serializer
            .vocabulary_mut()
            .insert_owned_literal(Literal::new(
                self.clone().into(),
                Type::Any(xsd_string).into(),
            ));
        serializer.insert(Term::Literal(literal))?;
        serializer.end()
    }
}
