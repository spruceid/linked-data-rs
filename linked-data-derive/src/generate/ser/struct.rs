use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;

use crate::generate::TypeAttributes;

use super::{variant_compound_fields, Error};

pub fn generate(
	attrs: &TypeAttributes,
	ident: Ident,
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

	let mut bounds = fields.visit.bounds;
	let visit = fields.visit.body;
	let vocabulary_bounds = fields.visit.vocabulary_bounds;

	let term = match fields.id_field {
		Some((field_access, ty)) => {
			bounds.push(quote! {
				#ty: ::linked_data::Interpret<V, I>
			});

			quote! {
				#field_access.interpret(interpretation, vocabulary)
			}
		}
		None => quote! {
			::linked_data::ResourceInterpretation::Uninterpreted(None)
		},
	};

	Ok(quote! {
		impl<V, I> ::linked_data::Interpret<V, I> for #ident
		where
			V: #vocabulary_bounds,
			I: ::linked_data::rdf_types::Interpretation,
			#(#bounds),*
		{
			fn interpret(
				&self,
				interpretation: &mut I,
				vocabulary: &mut V
			) -> linked_data::ResourceInterpretation<V, I> {
				#term
			}
		}

		impl<V, I> ::linked_data::LinkedDataSubject<V, I> for #ident
		where
			V: #vocabulary_bounds,
			I: ::linked_data::rdf_types::Interpretation,
			#(#bounds),*
		{
			fn visit_subject<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
			where
				S: ::linked_data::SubjectVisitor<V, I>
			{
				#visit
			}
		}

		impl<V, I> ::linked_data::LinkedDataPredicateObjects<V, I> for #ident
		where
			V: #vocabulary_bounds,
			I: ::linked_data::rdf_types::Interpretation,
			#(#bounds),*
		{
			fn visit_objects<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
			where
				S: ::linked_data::PredicateObjectsVisitor<V, I>
			{
				visitor.object(self)?;
				visitor.end()
			}
		}

		impl<V, I> ::linked_data::LinkedDataGraph<V, I> for #ident
		where
			V: #vocabulary_bounds,
			I: ::linked_data::rdf_types::Interpretation,
			#(#bounds),*
		{
			fn visit_graph<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
			where
				S: ::linked_data::GraphVisitor<V, I>
			{
				visitor.subject(self)?;
				visitor.end()
			}
		}

		impl<V, I> ::linked_data::LinkedData<V, I> for #ident
		where
			V: #vocabulary_bounds,
			I: ::linked_data::rdf_types::Interpretation,
			#(#bounds),*
		{
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
