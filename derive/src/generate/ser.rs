use proc_macro2::{Ident, TokenStream};
use quote::quote;
use syn::{spanned::Spanned, DeriveInput};

use super::{read_field_attributes, read_type_attributes, Error, TypeAttributes, VocabularyBounds};

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

#[derive(Default)]
pub struct FieldsVisitor {
	bounds: Vec<syn::WherePredicate>,
	vocabulary_bounds: VocabularyBounds,
	body: TokenStream,
}

#[derive(Default)]
pub struct CompoundFields {
	visit: FieldsVisitor,
	id_field: Option<(TokenStream, syn::Type)>,
}

fn variant_compound_fields(
	attrs: &TypeAttributes,
	fields: syn::Fields,
	named_accessor: impl Fn(Ident) -> TokenStream,
	unnamed_accessor: impl Fn(u32) -> TokenStream,
	by_ref: impl Fn(TokenStream) -> TokenStream,
) -> Result<CompoundFields, Error> {
	let mut id_field = None;
	let mut visit = FieldsVisitor::default();

	let mut visit_fields = Vec::new();

	for (i, field) in fields.into_iter().enumerate() {
		let span = field.span();
		let field_attrs = read_field_attributes(field.attrs)?;

		if !field_attrs.ignore {
			let field_access = match field.ident {
				Some(id) => named_accessor(id),
				None => unnamed_accessor(i as u32),
			};

			let ty = field.ty;

			if field_attrs.is_id {
				id_field = Some((field_access, ty));
				continue;
			}

			let field_ref = by_ref(field_access);
			let visit_field = if field_attrs.flatten {
				visit.bounds.push(
					syn::parse2(quote!(
						#ty: ::linked_data::LinkedDataSubject<I_, V_>
					))
					.unwrap(),
				);

				quote! {
					<#ty as ::linked_data::LinkedDataSubject<I_, V_>>::visit_subject(#field_ref, &mut visitor)?;
				}
			} else {
				match field_attrs.iri {
					Some(compact_iri) => {
						let iri = compact_iri.expand(&attrs.prefixes)?.into_string();
						visit.vocabulary_bounds.iri_mut = true;

						if field_attrs.graph_value {
							visit.bounds.push(
								syn::parse2(quote!(
									#ty: ::linked_data::LinkedDataGraph<I_, V_>
								))
								.unwrap(),
							);

							quote! {
								visitor.predicate(
									::linked_data::iref::Iri::new(#iri).unwrap(),
									&Some(::linked_data::AnonymousGraph(#field_ref))
								)?;
							}
						} else {
							visit.bounds.push(
								syn::parse2(quote!(
									#ty: ::linked_data::LinkedDataPredicateObjects<I_, V_>
								))
								.unwrap(),
							);

							quote! {
								visitor.predicate(
									::linked_data::iref::Iri::new(#iri).unwrap(),
									#field_ref
								)?;
							}
						}
					}
					None => return Err(Error::UnknownFieldSerializationMethod(span)),
				}
			};

			visit_fields.push(visit_field)
		}
	}

	visit.body = quote! {
		#(#visit_fields)*
		visitor.end()
	};

	Ok(CompoundFields { id_field, visit })
}
