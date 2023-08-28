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

	let mut bounds = fields.serialize.bounds;
	let serialize = fields.serialize.body;
	let vocabulary_bounds = fields.serialize.vocabulary_bounds;

	let term = match fields.id_field {
		Some((field_access, ty)) => {
			bounds.push(quote! {
				#ty: ::serde_ld::LexicalRepresentation<V, I>
			});

			quote! {
				#field_access.lexical_representation(interpretation, vocabulary)
			}
		}
		None => quote! {
			None
		},
	};

	Ok(quote! {
		impl<V, I> ::serde_ld::LexicalRepresentation<V, I> for #ident
		where
			V: #vocabulary_bounds,
			#(#bounds),*
		{
			fn lexical_representation(
				&self,
				interpretation: &mut I,
				vocabulary: &mut V
			) -> Option<::serde_ld::RdfTerm<V>> {
				#term
			}
		}

		impl<V, I> ::serde_ld::SerializeSubject<V, I> for #ident
		where
			V: #vocabulary_bounds,
			#(#bounds),*
		{
			fn serialize_subject<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
			where
				S: ::serde_ld::SubjectSerializer<V, I>
			{
				#serialize
			}
		}

		impl<V, I> ::serde_ld::SerializePredicate<V, I> for #ident
		where
			V: #vocabulary_bounds,
			#(#bounds),*
		{
			fn serialize_predicate<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
			where
				S: ::serde_ld::PredicateSerializer<V, I>
			{
				serializer.insert(self)?;
				serializer.end()
			}
		}

		impl<V, I> ::serde_ld::SerializeGraph<V, I> for #ident
		where
			V: #vocabulary_bounds,
			#(#bounds),*
		{
			fn serialize_graph<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
			where
				S: ::serde_ld::GraphSerializer<V, I>
			{
				serializer.insert(self)?;
				serializer.end()
			}
		}

		impl<V, I> ::serde_ld::SerializeLd<V, I> for #ident
		where
			V: #vocabulary_bounds,
			#(#bounds),*
		{
			fn serialize<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
			where
				S: ::serde_ld::Serializer<V, I>
			{
				serializer.insert_default(self)?;
				serializer.end()
			}
		}
	})
}
