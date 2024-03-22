use std::collections::HashMap;

use proc_macro2::TokenStream;

use quote::{format_ident, quote};
use syn::{spanned::Spanned, DeriveInput};

use super::{
	read_field_attributes, read_type_attributes, Error, InterpretationBounds, TypeAttributes,
};

mod r#enum;
mod r#struct;

pub fn subject(input: DeriveInput) -> Result<TokenStream, Error> {
	let attrs = read_type_attributes(input.attrs)?;
	match input.data {
		syn::Data::Struct(s) => r#struct::generate(&attrs, input.ident, input.generics, s),
		syn::Data::Enum(e) => r#enum::generate(&attrs, input.ident, input.generics, e),
		syn::Data::Union(u) => Err(Error::UnionType(u.union_token.span())),
	}
}

struct FieldsDeserialization {
	deserialize_fields: Vec<TokenStream>,
	constructor: TokenStream,
	interpretation_bounds: InterpretationBounds,
	bounds: Vec<syn::WherePredicate>,
}

fn generate_fields(
	attrs: &TypeAttributes,
	fields: syn::Fields,
) -> Result<FieldsDeserialization, Error> {
	let mut interpretation_bounds = InterpretationBounds::default();
	let mut bounds = Vec::new();
	let mut deserialize_fields = Vec::with_capacity(fields.len());
	let constructor = match fields {
		syn::Fields::Unit => quote!(),
		syn::Fields::Unnamed(fields) => {
			let mut fields_constructors = Vec::with_capacity(fields.unnamed.len());
			for (i, f) in fields.unnamed.into_iter().enumerate() {
				let ident = format_ident!("a{i}");
				match generate_field(
					&attrs.prefixes,
					f.attrs,
					&f.ty,
					&mut interpretation_bounds,
					&mut bounds,
				)? {
					Some(deserialize_field) => {
						deserialize_fields.push(quote! {
							let #ident = #deserialize_field ;
						});

						fields_constructors.push(quote!(#ident));
					}
					None => {
						let ty = &f.ty;
						fields_constructors
							.push(quote!(<#ty as ::std::default::Default>::default()));
					}
				}
			}

			quote!(( #(#fields_constructors),* ))
		}
		syn::Fields::Named(fields) => {
			let mut fields_constructors = Vec::with_capacity(fields.named.len());
			for f in fields.named {
				let ident = f.ident;

				match generate_field(
					&attrs.prefixes,
					f.attrs,
					&f.ty,
					&mut interpretation_bounds,
					&mut bounds,
				)? {
					Some(deserialize_field) => {
						deserialize_fields.push(quote! {
							let #ident = #deserialize_field ;
						});

						fields_constructors.push(quote!(#ident));
					}
					None => {
						let ty = &f.ty;
						fields_constructors
							.push(quote!(#ident: <#ty as ::std::default::Default>::default()));
					}
				}
			}

			quote!({ #(#fields_constructors),* })
		}
	};

	Ok(FieldsDeserialization {
		deserialize_fields,
		constructor,
		interpretation_bounds,
		bounds,
	})
}

fn generate_field(
	prefixes: &HashMap<String, String>,
	attrs: Vec<syn::Attribute>,
	ty: &syn::Type,
	interpretation_bounds: &mut InterpretationBounds,
	bounds: &mut Vec<syn::WherePredicate>,
) -> Result<Option<TokenStream>, Error> {
	let attrs = read_field_attributes(attrs)?;

	match attrs.iri {
		Some(compact_iri) => {
			let iri = compact_iri.expand(prefixes)?.into_string();
			interpretation_bounds.iri_mut = true;

			bounds.push(
				syn::parse2(
					quote!(#ty: ::linked_data::LinkedDataDeserializePredicateObjects<I_, V_>),
				)
				.unwrap(),
			);

			Ok(Some(quote! {
				match vocabulary_.get(unsafe { ::linked_data::iref::Iri::new_unchecked(#iri) }).and_then(|iri| interpretation_.iri_interpretation(&iri)) {
					Some(predicate_) => {
						let context_ = context_.with_predicate(&predicate_);
						::linked_data::LinkedDataDeserializePredicateObjects::deserialize_objects_in(
							vocabulary_,
							interpretation_,
							dataset_,
							graph_,
							::linked_data::rdf_types::dataset::PatternMatchingDataset::quad_objects(dataset_, graph_, resource_, &predicate_),
							context_
						)?
					}
					None => {
						let context_ = context_.with_predicate_iri(unsafe {::linked_data::iref::Iri::new_unchecked(#iri) });
						::linked_data::LinkedDataDeserializePredicateObjects::deserialize_objects_in(
							vocabulary_,
							interpretation_,
							dataset_,
							graph_,
							[],
							context_
						)?
					}
				}
			}))
		}
		None => {
			if attrs.is_id || attrs.flatten {
				bounds.push(
					syn::parse2(quote!(#ty: ::linked_data::LinkedDataDeserializeSubject<I_, V_>))
						.unwrap(),
				);

				Ok(Some(quote! {
					::linked_data::LinkedDataDeserializeSubject::deserialize_subject_in(
						vocabulary_,
						interpretation_,
						dataset_,
						graph_,
						resource_,
						context_
					)?
				}))
			} else if attrs.ignore {
				Ok(None)
			} else {
				panic!()
			}
		}
	}
}
