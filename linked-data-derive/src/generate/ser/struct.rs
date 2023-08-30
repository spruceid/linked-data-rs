use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote, format_ident};
use syn::punctuated::Punctuated;

use crate::generate::{TypeAttributes, extend_generics};

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
			bounds.push(syn::parse2(quote! {
				#ty: ::linked_data::Interpret<V, I>
			}).unwrap());

			quote! {
				#field_access.interpret(vocabulary, interpretation)
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
		impl #impl_generics ::linked_data::Interpret<V, I> for #ident #ty_generics #where_clause {
			fn interpret(
				&self,
				vocabulary: &mut V,
				interpretation: &mut I
			) -> linked_data::ResourceInterpretation<V, I> {
				#term
			}
		}

		impl #impl_generics ::linked_data::LinkedDataSubject<V, I> for #ident #ty_generics #where_clause {
			fn visit_subject<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
			where
				S: ::linked_data::SubjectVisitor<V, I>
			{
				#visit
			}
		}

		impl #impl_generics ::linked_data::LinkedDataPredicateObjects<V, I> for #ident #ty_generics #where_clause {
			fn visit_objects<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
			where
				S: ::linked_data::PredicateObjectsVisitor<V, I>
			{
				visitor.object(self)?;
				visitor.end()
			}
		}

		impl #impl_generics ::linked_data::LinkedDataGraph<V, I> for #ident #ty_generics #where_clause {
			fn visit_graph<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
			where
				S: ::linked_data::GraphVisitor<V, I>
			{
				visitor.subject(self)?;
				visitor.end()
			}
		}

		impl #impl_generics ::linked_data::LinkedData<V, I> for #ident #ty_generics #where_clause {
			fn visit<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
			where
				S: ::linked_data::Visitor<V, I>
			{
				visitor.default_graph(self)?;
				visitor.end()
			}
		}
	})
}
