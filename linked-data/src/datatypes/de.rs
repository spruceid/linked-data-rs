use rdf_types::{
	Interpretation, IriVocabulary, LanguageTagVocabulary, LiteralVocabulary,
	ReverseIriInterpretation, ReverseLiteralInterpretation, Vocabulary, RDF_LANG_STRING,
};

use crate::{
	Context, FromLinkedDataError, LinkedDataDeserializePredicateObjects,
	LinkedDataDeserializeSubject,
};

macro_rules! deserialize_datatype {
	($($ty:ty : $iri:ident),*) => {
		$(
			impl<V: Vocabulary, I: Interpretation> LinkedDataDeserializeSubject<I, V> for $ty
			where
				V: LiteralVocabulary<Type = rdf_types::literal::Type<
					<V as IriVocabulary>::Iri,
					<V as LanguageTagVocabulary>::LanguageTag,
				>>,
				V::Value: AsRef<str>,
				I: ReverseIriInterpretation<Iri = V::Iri> + ReverseLiteralInterpretation<Literal = V::Literal>
			{
				fn deserialize_subject_in<D>(
					vocabulary: &V,
					interpretation: &I,
					_dataset: &D,
					_graph: &D::Graph,
					resource: &I::Resource,
					context: Context<I>
				) -> Result<Self, FromLinkedDataError>
				where
					D: grdf::Dataset<Subject = I::Resource, Predicate = I::Resource, Object = I::Resource, GraphLabel = I::Resource>
				{
					let mut literal_ty = None;
					for l in interpretation.literals_of(resource) {
						let l = vocabulary.literal(l).unwrap();
						match l.type_() {
							rdf_types::literal::Type::Any(ty_iri) => {
								let ty_iri = vocabulary.iri(ty_iri).unwrap();
								if ty_iri == xsd_types::$iri {
									return match l.value().as_ref().parse() {
										Ok(value) => Ok(value),
										Err(_) => Err(FromLinkedDataError::InvalidLiteral(
											context.into_iris(
												vocabulary,
												interpretation
											)
										))
									}
								}

								literal_ty = Some(ty_iri)
							}
							rdf_types::literal::Type::LangString(_) => {
								literal_ty = Some(RDF_LANG_STRING)
							}
						}
					}

					match literal_ty {
						Some(ty) => {
							Err(FromLinkedDataError::LiteralTypeMismatch {
								context: context.into_iris(vocabulary, interpretation),
								expected: Some(xsd_types::$iri.to_owned()),
								found: ty.to_owned()
							})
						}
						None => {
							Err(FromLinkedDataError::ExpectedLiteral(
								context.into_iris(vocabulary, interpretation)
							))
						}
					}
				}
			}

			impl<V: Vocabulary, I: Interpretation> LinkedDataDeserializePredicateObjects<I, V> for $ty
			where
				V: LiteralVocabulary<Type = rdf_types::literal::Type<
					<V as IriVocabulary>::Iri,
					<V as LanguageTagVocabulary>::LanguageTag,
				>>,
				V::Value: AsRef<str>,
				I: ReverseIriInterpretation<Iri = V::Iri> + ReverseLiteralInterpretation<Literal = V::Literal>
			{
				fn deserialize_objects_in<'a, D>(
					vocabulary: &V,
					interpretation: &I,
					dataset: &D,
					graph: &D::Graph,
					objects: impl IntoIterator<Item = &'a <I as Interpretation>::Resource>,
					context: Context<I>
				) -> Result<Self, FromLinkedDataError>
				where
					<I as Interpretation>::Resource: 'a,
					D: grdf::Dataset<Subject = <I as Interpretation>::Resource, Predicate = <I as Interpretation>::Resource, Object = <I as Interpretation>::Resource, GraphLabel = <I as Interpretation>::Resource>
				{
					let mut error = None;

					for o in objects {
						match Self::deserialize_subject_in(vocabulary, interpretation, dataset, graph, o, context) {
							Ok(value) => return Ok(value),
							Err(e) => error = Some(e)
						}
					}

					Err(error.unwrap_or_else(|| {
						FromLinkedDataError::MissingRequiredValue(
							context.into_iris(vocabulary, interpretation)
						)
					}))
				}
			}
		)*
	};
}

deserialize_datatype!(
	u8: XSD_UNSIGNED_BYTE,
	u16: XSD_UNSIGNED_SHORT,
	u32: XSD_UNSIGNED_INT,
	u64: XSD_UNSIGNED_LONG,
	i8: XSD_BYTE,
	i16: XSD_SHORT,
	i32: XSD_INT,
	i64: XSD_LONG,
	String: XSD_STRING,
	xsd_types::DateTime: XSD_DATE_TIME
);
