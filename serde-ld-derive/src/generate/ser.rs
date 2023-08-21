use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{spanned::Spanned, DeriveInput};

use super::{read_field_attributes, read_type_attributes, Error};

pub fn subject(input: DeriveInput) -> Result<TokenStream, Error> {
    let attrs = read_type_attributes(input.attrs)?;
    match input.data {
        syn::Data::Struct(s) => {
            let mut serialize_fields = Vec::with_capacity(s.fields.len());

            let mut requires_mut_vocabulary = false;
            let mut bounds = Vec::new();

            let mut id_field = None;

            for (i, field) in s.fields.into_iter().enumerate() {
                let span = field.span();
                let field_attrs = read_field_attributes(field.attrs)?;
                let field_id: TokenStream = match field.ident {
                    Some(id) => quote!(#id),
                    None => {
                        let index = syn::Index {
                            index: i as u32,
                            span: Span::call_site(),
                        };
                        quote!(#index)
                    }
                };
                let ty = field.ty;

                if field_attrs.is_id {
                    id_field = Some((field_id, ty));
                    continue;
                }

                let serialize_field = if field_attrs.flatten {
                    bounds.push(quote!(
                        #ty: ::serde_ld::SerializeSubject<V, I>
                    ));

                    quote! {
                        <#ty as ::serde_ld::SerializeSubject<V, I>>::serialize_subject(&self.#field_id, &mut serializer)?;
                    }
                } else {
                    match field_attrs.iri {
                        Some(compact_iri) => {
                            let iri = compact_iri.expand(&attrs.prefixes)?.into_string();
                            requires_mut_vocabulary = true;
                            bounds.push(quote!(
                                #ty: ::serde_ld::SerializePredicate<V, I>
                            ));

                            quote! {
                                serializer.insert(
                                    ::serde_ld::iref::Iri::new(#iri).unwrap(),
                                    &self.#field_id
                                )?;
                            }
                        }
                        None => return Err(Error::UnknownFieldSerializationMethod(span)),
                    }
                };

                serialize_fields.push(serialize_field)
            }

            let ident = input.ident;

            let base_vocabulary_bound = if requires_mut_vocabulary {
                quote!(::serde_ld::rdf_types::Vocabulary + ::serde_ld::rdf_types::IriVocabularyMut)
            } else {
                quote!(::serde_ld::rdf_types::Vocabulary)
            };

            let term = match id_field {
                Some((field_id, ty)) => {
                    bounds.push(quote! {
                        #ty: ::serde_ld::LexicalRepresentation<V, I>
                    });

                    quote! {
                        self.#field_id.lexical_representation(interpretation, vocabulary, generator)
                    }
                }
                None => quote! {
                    ::serde_ld::Anonymous.lexical_representation(vocabulary, generator)
                },
            };

            Ok(quote! {
                impl<V, I> ::serde_ld::LexicalRepresentation<V, I> for #ident
                where
                    V: #base_vocabulary_bound,
                    #(#bounds),*
                {
                    fn lexical_representation(
                        &self,
                        interpretation: &mut I,
                        vocabulary: &mut V,
                        generator: &mut impl ::serde_ld::rdf_types::Generator<V>,
                    ) -> ::serde_ld::rdf_types::Term<::serde_ld::rdf_types::Id<V::Iri, V::BlankId>, V::Literal> {
                        #term
                    }
                }

                impl<V, I> ::serde_ld::SerializeSubject<V, I> for #ident
                where
                    V: #base_vocabulary_bound,
                    #(#bounds),*
                {
                    fn serialize_subject<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
                    where
                        S: ::serde_ld::SubjectSerializer<V, I>
                    {
                        #(#serialize_fields)*
                        serializer.end()
                    }
                }

                impl<V, I> ::serde_ld::SerializePredicate<V, I> for #ident
                where
                    V: #base_vocabulary_bound,
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
                    V: #base_vocabulary_bound,
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
                    V: #base_vocabulary_bound,
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
        syn::Data::Enum(_e) => {
            todo!()
        }
        syn::Data::Union(u) => Err(Error::UnionType(u.union_token.span())),
    }
}
