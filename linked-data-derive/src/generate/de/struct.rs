use proc_macro2::{Ident, TokenStream};
use quote::quote;

use crate::generate::{TypeAttributes, extend_generics, VocabularyBounds};

use super::{Error, FieldsDeserialization, generate_fields};

pub fn generate(
	attrs: &TypeAttributes,
	ident: Ident,
	generics: syn::Generics,
	s: syn::DataStruct,
) -> Result<TokenStream, Error> {
	let FieldsDeserialization {
		deserialize_fields,
		constructor,
		interpretation_bounds,
		bounds
	} = generate_fields(attrs, s.fields)?;

	let vocabulary_bounds = VocabularyBounds::default();
	let ld_generics = extend_generics(&generics, vocabulary_bounds, interpretation_bounds, bounds);
	let (_, ty_generics, _) = generics.split_for_impl();
	let (impl_generics, _, where_clause) = ld_generics.split_for_impl();

	Ok(quote! {
		impl #impl_generics ::linked_data::LinkedDataDeserializeSubject<V_, I_> for #ident #ty_generics #where_clause {
			fn deserialize_subject<D_>(
				vocabulary_: &V_,
				interpretation_: &I_,
				dataset_: &D_,
				graph_: &D_::Graph,
				resource_: &I_::Resource
			) -> Result<Self, ::linked_data::FromLinkedDataError>
			where
				D_: ::linked_data::grdf::Dataset<Subject = I_::Resource, Predicate = I_::Resource, Object = I_::Resource, GraphLabel = I_::Resource>
			{
				#(#deserialize_fields)*
				Ok(Self #constructor)
			}
		}

		impl #impl_generics ::linked_data::LinkedDataDeserializePredicateObjects<V_, I_> for #ident #ty_generics #where_clause {
			fn deserialize_objects<'a, D_>(
				vocabulary: &V_,
				interpretation: &I_,
				dataset: &D_,
				graph: &D_::Graph,
				objects: impl IntoIterator<Item = &'a I_::Resource>
			) -> Result<Self, ::linked_data::FromLinkedDataError>
			where
				I_::Resource: 'a,
				D_: ::linked_data::grdf::Dataset<Subject = I_::Resource, Predicate = I_::Resource, Object = I_::Resource, GraphLabel = I_::Resource>
			{
				let mut objects = objects.into_iter();

				match objects.next() {
					Some(object) => {
						let value = <Self as ::linked_data::LinkedDataDeserializeSubject<V_, I_>>::deserialize_subject(
							vocabulary,
							interpretation,
							dataset,
							graph,
							object
						)?;

						if objects.next().is_some() {
							Err(::linked_data::FromLinkedDataError::TooManyValues)
						} else {
							Ok(value)
						}
					}
					None => {
						Err(::linked_data::FromLinkedDataError::MissingRequiredValue)
					}
				}
			}
		}
	})
}
