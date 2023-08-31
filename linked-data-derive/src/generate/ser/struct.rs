use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;

use crate::generate::{extend_generics, TypeAttributes};

use super::{variant_compound_fields, Error};

pub fn generate(
	attrs: &TypeAttributes,
	ident: Ident,
	generics: syn::Generics,
	s: syn::DataStruct,
) -> Result<TokenStream, Error> {
	let fields = variant_compound_fields(
		attrs,
		s.fields,
		|f| quote!(self.#f),
		|i| {
			let index = syn::Index {
				index: i,
				span: Span::call_site(),
			};

			quote!(self.#index)
		},
		|t| quote!(&#t),
	)?;

	let mut bounds: Vec<syn::WherePredicate> = fields.visit.bounds;
	let visit = fields.visit.body;
	let vocabulary_bounds = fields.visit.vocabulary_bounds;

	let term = match fields.id_field {
		Some((field_access, ty)) => {
			bounds.push(
				syn::parse2(quote! {
					#ty: ::linked_data::LinkedDataResource<V_, I_>
				})
				.unwrap(),
			);

			quote! {
				#field_access.interpretation(vocabulary, interpretation)
			}
		}
		None => quote! {
			::linked_data::ResourceInterpretation::Uninterpreted(None)
		},
	};

	let ld_generics = extend_generics(&generics, vocabulary_bounds, bounds);
	let (_, ty_generics, _) = generics.split_for_impl();
	let (impl_generics, _, where_clause) = ld_generics.split_for_impl();

	Ok(quote! {
		impl #impl_generics ::linked_data::LinkedDataResource<V_, I_> for #ident #ty_generics #where_clause {
			fn interpretation(
				&self,
				vocabulary: &mut V_,
				interpretation: &mut I_
			) -> linked_data::ResourceInterpretation<V_, I_> {
				#term
			}
		}

		impl #impl_generics ::linked_data::LinkedDataSubject<V_, I_> for #ident #ty_generics #where_clause {
			fn visit_subject<S_>(&self, mut visitor: S_) -> Result<S_::Ok, S_::Error>
			where
				S_: ::linked_data::SubjectVisitor<V_, I_>
			{
				#visit
			}
		}

		impl #impl_generics ::linked_data::LinkedDataPredicateObjects<V_, I_> for #ident #ty_generics #where_clause {
			fn visit_objects<S_>(&self, mut visitor: S_) -> Result<S_::Ok, S_::Error>
			where
				S_: ::linked_data::PredicateObjectsVisitor<V_, I_>
			{
				visitor.object(self)?;
				visitor.end()
			}
		}

		impl #impl_generics ::linked_data::LinkedDataGraph<V_, I_> for #ident #ty_generics #where_clause {
			fn visit_graph<S_>(&self, mut visitor: S_) -> Result<S_::Ok, S_::Error>
			where
				S_: ::linked_data::GraphVisitor<V_, I_>
			{
				visitor.subject(self)?;
				visitor.end()
			}
		}

		impl #impl_generics ::linked_data::LinkedData<V_, I_> for #ident #ty_generics #where_clause {
			fn visit<S_>(&self, mut visitor: S_) -> Result<S_::Ok, S_::Error>
			where
				S_: ::linked_data::Visitor<V_, I_>
			{
				visitor.default_graph(self)?;
				visitor.end()
			}
		}
	})
}
