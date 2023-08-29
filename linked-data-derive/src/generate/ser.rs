use proc_macro2::{TokenStream, Ident};
use quote::{ToTokens, quote};
use syn::{DeriveInput, spanned::Spanned};

use super::{Error, read_type_attributes, TypeAttributes, read_field_attributes};

mod r#enum;
mod r#struct;

pub fn subject(input: DeriveInput) -> Result<TokenStream, Error> {
	let attrs = read_type_attributes(input.attrs)?;
	match input.data {
		syn::Data::Struct(s) => r#struct::generate(&attrs, input.ident, s),
		syn::Data::Enum(e) => r#enum::generate(&attrs, input.ident, e),
		syn::Data::Union(u) => Err(Error::UnionType(u.union_token.span())),
	}
}

#[derive(Default, Clone, Copy)]
pub struct VocabularyBounds {
	iri_mut: bool,
}

impl VocabularyBounds {
	pub fn add(&mut self, other: Self) {
		self.iri_mut |= other.iri_mut
	}
}

impl ToTokens for VocabularyBounds {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		tokens.extend(quote! {
			::linked_data::rdf_types::Vocabulary
		});

		if self.iri_mut {
			tokens.extend(quote! {
				+ ::linked_data::rdf_types::IriVocabularyMut
			})
		}
	}
}

#[derive(Default)]
pub struct FieldsVisitor {
	bounds: Vec<TokenStream>,
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
			visit.bounds.push(quote!(
				#ty: ::linked_data::LinkedDataSubject<V, I>
			));

			quote! {
				<#ty as ::linked_data::LinkedDataSubject<V, I>>::visit_subject(#field_ref, &mut visitor)?;
			}
		} else {
			match field_attrs.iri {
				Some(compact_iri) => {
					let iri = compact_iri.expand(&attrs.prefixes)?.into_string();
					visit.vocabulary_bounds.iri_mut = true;
					visit.bounds.push(quote!(
						#ty: ::linked_data::LinkedDataPredicateObjects<V, I>
					));

					quote! {
						visitor.predicate(
							::linked_data::iref::Iri::new(#iri).unwrap(),
							#field_ref
						)?;
					}
				}
				None => return Err(Error::UnknownFieldSerializationMethod(span)),
			}
		};

		visit_fields.push(visit_field)
	}

	visit.body = quote! {
		#(#visit_fields)*
		visitor.end()
	};

	Ok(CompoundFields { id_field, visit })
}
