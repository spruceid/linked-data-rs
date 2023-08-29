use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::spanned::Spanned;

use crate::generate::{TypeAttributes, read_field_attributes};

use super::Error;

fn generate_field(
	attrs: Vec<syn::Attribute>,
	ty: &syn::Type,
	cases: &mut Vec<TokenStream>
) -> Result<TokenStream, Error> {
	let attrs = read_field_attributes(attrs)?;
	
	// let iri = attrs.iri

	// Ok(quote! {
	// 	if iri == #iri {
	// 		// ...
	// 	}
	// })
	todo!()
}

pub fn generate(
	attrs: &TypeAttributes,
	ident: Ident,
	s: syn::DataStruct,
) -> Result<TokenStream, Error> {
	let mut cases = Vec::with_capacity(s.fields.len());
	let data_struct = match s.fields {
		syn::Fields::Unit => quote!(struct Data;),
		syn::Fields::Unnamed(fields) => {
			let mut opt_fields = Vec::with_capacity(fields.unnamed.len());
			for (i, f) in fields.unnamed.into_iter().enumerate() {
				// let index = syn::Index {
				// 	index: i as u32,
				// 	span: f.span()
				// };
				let ty = f.ty;
				generate_field(f.attrs, &ty, &mut cases)?;
				opt_fields.push(quote!(#ty));
			}

			quote!(struct Data( #(#opt_fields),* );)
		}
		syn::Fields::Named(fields) => {
			let opt_fields = fields.named.into_iter().map(|f| {
				let ident = f.ident;
				let ty = f.ty;
				quote!(#ident: #ty)
			});

			quote!(struct Data {
				#(#opt_fields),*
			})
		}
	};

	Ok(quote! {
		impl<V, I> ::linked_data::LinkedDataDeserializeSubject<V, I> for #ident {
			fn deserialize_subject(
				vocabulary: &V,
				interpretation: &I,
				subject: &impl LinkedDataSubject<V, I>
			) -> Result<Self, FromLinkedDataError> {
				#data_struct

				struct Visitor {
					vocabulary: &V,
					interpretation: &I,
					data: Data
				}

				impl<V, I> SubjectVisitor<V, I> for Visitor<V, I> {
					fn predicate(
						&mut self,
						predicate: &P,
						objects: &O
					) {
						let repr = predicate.lexical_representation(
							self.vocabulary,
							self.interpretation
						);

						if let Some(Term::Id(Id::Iri(iri))) = repr {
							let iri = vocabulary.iri(&iri).unwrap();
							#(#cases)else*
						}

						Ok(Self {
							
						})
					}
				}

				subject.visit_subject(Visitor {
					vocabulary,
					interpretation
				})
			}
		}
	})
}
