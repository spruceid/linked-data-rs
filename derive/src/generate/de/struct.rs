use proc_macro2::{Ident, TokenStream};
use quote::quote;

use crate::generate::{extend_generics, TypeAttributes, VocabularyBounds};

use super::{generate_fields, Error, FieldsDeserialization};

pub fn generate(
	attrs: &TypeAttributes,
	ident: Ident,
	generics: syn::Generics,
	s: syn::DataStruct,
) -> Result<TokenStream, Error> {
	let FieldsDeserialization {
		deserialize_fields,
		constructor,
		mut interpretation_bounds,
		bounds,
	} = generate_fields(attrs, s.fields)?;

	interpretation_bounds.reverse_iri = true;

	let vocabulary_bounds = VocabularyBounds::default();
	let ld_generics = extend_generics(&generics, vocabulary_bounds, interpretation_bounds, bounds);
	let (_, ty_generics, _) = generics.split_for_impl();
	let (impl_generics, _, where_clause) = ld_generics.split_for_impl();

	Ok(quote! {
		impl #impl_generics ::linked_data::LinkedDataDeserializeSubject<I_, V_> for #ident #ty_generics #where_clause {
			fn deserialize_subject_in<D_>(
				vocabulary_: &V_,
				interpretation_: &I_,
				dataset_: &D_,
				graph_: Option<&I_::Resource>,
				resource_: &I_::Resource,
				context_: ::linked_data::Context<I_>
			) -> Result<Self, ::linked_data::FromLinkedDataError>
			where
				D_: ::linked_data::rdf_types::dataset::PatternMatchingDataset<Resource = I_::Resource>
			{
				let context_ = context_.with_subject(resource_);
				#(#deserialize_fields)*
				Ok(Self #constructor)
			}
		}

		impl #impl_generics ::linked_data::LinkedDataDeserializePredicateObjects<I_, V_> for #ident #ty_generics #where_clause {
			fn deserialize_objects_in<'de_, D_>(
				vocabulary: &V_,
				interpretation: &I_,
				dataset: &D_,
				graph: Option<&I_::Resource>,
				objects: impl IntoIterator<Item = &'de_ I_::Resource>,
				context: ::linked_data::Context<I_>
			) -> Result<Self, ::linked_data::FromLinkedDataError>
			where
				I_::Resource: 'de_,
				D_: ::linked_data::rdf_types::dataset::PatternMatchingDataset<Resource = I_::Resource>
			{
				let mut objects = objects.into_iter();

				match objects.next() {
					Some(object) => {
						let value = <Self as ::linked_data::LinkedDataDeserializeSubject<I_, V_>>::deserialize_subject_in(
							vocabulary,
							interpretation,
							dataset,
							graph,
							object,
							context
						)?;

						if objects.next().is_some() {
							Err(::linked_data::FromLinkedDataError::TooManyValues(
								context.into_iris(vocabulary, interpretation)
							))
						} else {
							Ok(value)
						}
					}
					None => {
						Err(::linked_data::FromLinkedDataError::MissingRequiredValue(
							context.into_iris(vocabulary, interpretation)
						))
					}
				}
			}
		}
	})
}
