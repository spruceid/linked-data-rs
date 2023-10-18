use proc_macro2::{Ident, TokenStream};
use quote::quote;

use super::{generate_fields, Error};
use crate::generate::{
	extend_generics, read_variant_attributes, InterpretationBounds, TypeAttributes,
	VocabularyBounds,
};

pub fn generate(
	attrs: &TypeAttributes,
	ident: Ident,
	generics: syn::Generics,
	e: syn::DataEnum,
) -> Result<TokenStream, Error> {
	let mut interpretation_bounds = InterpretationBounds::default();
	let mut bounds = Vec::new();

	let mut deserialize_variants = Vec::with_capacity(e.variants.len());
	for v in e.variants {
		let v_ident = &v.ident;
		let v_attrs = read_variant_attributes(v.attrs)?;

		let deserialize_variant = match v_attrs.iri {
			Some(compact_iri) => {
				let iri = compact_iri.expand(&attrs.prefixes)?.into_string();
				interpretation_bounds.iri_mut = true;

				match variant_shape(&v.fields) {
					VariantShape::Simple(ty) => {
						bounds.push(
							syn::parse2(
								quote!(#ty: ::linked_data::LinkedDataDeserializePredicateObjects<I_, V_>),
							)
							.unwrap(),
						);

						quote! {
							match vocabulary_.get(unsafe { ::linked_data::iref::Iri::new_unchecked(#iri) }).and_then(|iri| interpretation_.iri_interpretation(&iri)) {
								Some(predicate) => {
									let result = ::linked_data::LinkedDataDeserializePredicateObjects::deserialize_objects(
										vocabulary_,
										interpretation_,
										dataset_,
										graph_,
										<D_::Graph as ::linked_data::grdf::Graph>::objects(graph_, resource_, &predicate)
									);

									match result {
										Ok(value) => return Ok(Self::#v_ident(value)),
										Err(e) => error = e
									}
								}
								None => {
									error = ::linked_data::FromLinkedDataError::MissingRequiredValue
								}
							}
						}
					}
					VariantShape::Compound => {
						let fields_de = generate_fields(attrs, v.fields)?;
						interpretation_bounds.add(fields_de.interpretation_bounds);
						bounds.extend(fields_de.bounds);

						let deserialize_fields = fields_de.deserialize_fields;
						let constructor = fields_de.constructor;

						quote! {
							match vocabulary_.get(unsafe { ::linked_data::iref::Iri::new_unchecked(#iri) }).and_then(|iri| interpretation_.iri_interpretation(&iri)) {
								Some(predicate) => {
									let mut objects = <D_::Graph as ::linked_data::grdf::Graph>::objects(graph_, resource_, &predicate);

									let result = match objects.next() {
										Some(resource_) => {
											(|| {
												#(#deserialize_fields)*

												if objects.next().is_some() {
													Err(::linked_data::FromLinkedDataError::TooManyValues)
												} else {
													Ok(Self::#v_ident #constructor)
												}
											})()
										}
										None => Err(::linked_data::FromLinkedDataError::MissingRequiredValue)
									};

									match result {
										Ok(value) => return Ok(value),
										Err(e) => error = e
									}
								}
								None => {
									error = ::linked_data::FromLinkedDataError::MissingRequiredValue
								}
							}
						}
					}
					VariantShape::Unit => {
						interpretation_bounds.reverse_iri = true;
						quote! {
							let expected_iri_ = unsafe { ::linked_data::iref::Iri::new_unchecked(#iri) };
							for i in interpretation_.iris_of(resource_) {
								let iri_ = vocabulary_.iri(&i).unwrap();
								if iri_ == expected_iri_ {
									return Ok(Self::#v_ident)
								}
							}
						}
					}
				}
			}
			None => match variant_shape(&v.fields) {
				VariantShape::Simple(_ty) => {
					quote! {
						let result = ::linked_data::LinkedDataDeserializeSubject::deserialize_subject(
							vocabulary_,
							interpretation_,
							dataset_,
							graph_,
							resource_
						);

						match result {
							Ok(value) => return Ok(value),
							Err(e) => error = e
						}
					}
				}
				VariantShape::Compound => {
					let variant_de = generate_fields(attrs, v.fields)?;
					interpretation_bounds.add(variant_de.interpretation_bounds);
					bounds.extend(variant_de.bounds);

					let deserialize_fields = variant_de.deserialize_fields;
					let constructor = variant_de.constructor;

					quote! {
						let result = (|| {
							#(#deserialize_fields)*
							Ok(Self::#v_ident #constructor)
						})();

						match result {
							Ok(value) => return Ok(value),
							Err(e) => error = e
						}
					}
				}
				VariantShape::Unit => {
					panic!()
				}
			},
		};

		deserialize_variants.push(deserialize_variant)
	}

	let vocabulary_bounds = VocabularyBounds::default();
	let ld_generics = extend_generics(&generics, vocabulary_bounds, interpretation_bounds, bounds);
	let (_, ty_generics, _) = generics.split_for_impl();
	let (impl_generics, _, where_clause) = ld_generics.split_for_impl();

	Ok(quote! {
		impl #impl_generics ::linked_data::LinkedDataDeserializeSubject<I_, V_> for #ident #ty_generics #where_clause {
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
				let mut error = ::linked_data::FromLinkedDataError::InvalidSubject;

				#(#deserialize_variants)*

				Err(error)
			}
		}

		impl #impl_generics ::linked_data::LinkedDataDeserializePredicateObjects<I_, V_> for #ident #ty_generics #where_clause {
			fn deserialize_objects<'de_, D_>(
				vocabulary: &V_,
				interpretation: &I_,
				dataset: &D_,
				graph: &D_::Graph,
				objects: impl IntoIterator<Item = &'de_ I_::Resource>
			) -> Result<Self, ::linked_data::FromLinkedDataError>
			where
				I_::Resource: 'de_,
				D_: ::linked_data::grdf::Dataset<Subject = I_::Resource, Predicate = I_::Resource, Object = I_::Resource, GraphLabel = I_::Resource>
			{
				let mut objects = objects.into_iter();

				match objects.next() {
					Some(object) => {
						let value = <Self as ::linked_data::LinkedDataDeserializeSubject<I_, V_>>::deserialize_subject(
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

enum VariantShape<'a> {
	Simple(&'a syn::Type),
	Compound,
	Unit,
}

fn variant_shape(fields: &syn::Fields) -> VariantShape {
	match fields {
		syn::Fields::Named(_) => VariantShape::Compound,
		syn::Fields::Unnamed(unnamed_fields) => {
			let mut fields_iter = unnamed_fields.unnamed.iter();

			if let Some(field) = fields_iter.next() {
				if fields_iter.next().is_none()
					&& !field.attrs.iter().any(|attr| attr.path().is_ident("ld"))
				{
					return VariantShape::Simple(&field.ty);
				}
			}

			VariantShape::Compound
		}
		syn::Fields::Unit => VariantShape::Unit,
	}
}
