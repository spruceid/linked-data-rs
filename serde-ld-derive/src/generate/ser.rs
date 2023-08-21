use iref::IriBuf;
use proc_macro2::{Span, TokenStream, Ident};
use quote::{quote, format_ident, ToTokens};
use syn::{spanned::Spanned, DeriveInput};

use super::{read_field_attributes, read_type_attributes, Error, TypeAttributes, read_variant_attributes, VariantAttributes};

pub fn subject(input: DeriveInput) -> Result<TokenStream, Error> {
    let attrs = read_type_attributes(input.attrs)?;
    match input.data {
        syn::Data::Struct(s) => {
            let fields = fields(
                &attrs,
                s.fields,
                |f| quote!(self.#f), 
                |i| {
                    let index = syn::Index {
                        index: i as u32,
                        span: Span::call_site(),
                    };
                    
                    quote!(self.#index)
                },
                |t| quote!(&#t)
            )?;
            let mut bounds = fields.serialize.bounds;
            let serialize = fields.serialize.body;
            let vocabulary_bounds = fields.serialize.vocabulary_bounds;

            let ident = input.ident;

            let term = match fields.id_field {
                Some((field_access, ty)) => {
                    bounds.push(quote! {
                        #ty: ::serde_ld::LexicalRepresentation<V, I>
                    });

                    quote! {
                        #field_access.lexical_representation(interpretation, vocabulary, generator)
                    }
                }
                None => quote! {
                    ::serde_ld::Anonymous.lexical_representation(vocabulary, generator)
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
                        vocabulary: &mut V,
                        generator: &mut impl ::serde_ld::rdf_types::Generator<V>,
                    ) -> ::serde_ld::rdf_types::Term<::serde_ld::rdf_types::Id<V::Iri, V::BlankId>, V::Literal> {
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
        syn::Data::Enum(e) => {
            let ident = input.ident;

            let mut serialize_bounds = Vec::new();
            let mut serialize_vocabulary_bounds = VocabularyBounds::default();
            let mut serialize_cases = Vec::new();

            let mut nested = false;
            let mut last_variant_span = None;

            for variant in e.variants {
                let span = variant.span();
                let variant_attrs = read_variant_attributes(variant.attrs)?;

                let variant_id = variant.ident;

                let input = match &variant.fields {
                    syn::Fields::Named(fields) => {
                        let names = fields.named.iter().map(|f| &f.ident);
                        quote! {
                            { #(#names),* }
                        }
                    }
                    syn::Fields::Unnamed(fields) => {
                        let names = (0..fields.unnamed.len()).map(|i| format_ident!("a{i}"));
                        quote! {
                            ( #(#names),* )
                        }
                    }
                    syn::Fields::Unit => {
                        TokenStream::new()
                    }
                };

                let shape = variant_shape(&variant.fields);
                let case = match variant_nest(&attrs, &variant_attrs)? {
                    Some(iri) => {
                        if !nested {
                            if let Some(span) = last_variant_span {
                                return Err(Error::MissingVariantIri(span))
                            }

                            nested = true;
                            serialize_vocabulary_bounds.iri_mut = true;
                        }

                        let iri = iri.as_str();

                        match shape {
                            VariantShape::Simple(id, ty) => {
                                serialize_bounds.push(quote! {
                                    #ty: ::serde_ld::SerializePredicate<V, I>
                                });

                                quote! {
                                    serializer.insert(
                                        ::serde_ld::iref::Iri::new(#iri).unwrap(),
                                        #id
                                    )?;
                                    serializer.end()
                                }
                            }
                            VariantShape::Compound => {
                                let borrowed_fields = match &variant.fields {
                                    syn::Fields::Named(fields) => {
                                        let fields = fields.named.iter().map(|f| {
                                            let id = &f.ident;
                                            let ty = &f.ty;
                                            quote!(#id: &'_nest #ty)
                                        });

                                        quote! {
                                            { #(#fields),* }
                                        }
                                    }
                                    syn::Fields::Unnamed(fields) => {
                                        let fields = fields.unnamed.iter().map(|f| {
                                            let ty = &f.ty;
                                            quote!(&'_nest #ty)
                                        });

                                        quote! {
                                            ( #(#fields),* )
                                        }
                                    }
                                    syn::Fields::Unit => quote!()
                                };

                                let fields = fields(
                                    &attrs,
                                    variant.fields, 
                                    |f| quote!(#f), 
                                    |i| {
                                        let ident = format_ident!("a{i}");
                                        quote!(#ident)
                                    },
                                    |t| t
                                )?;

                                let nest_bounds = fields.serialize.bounds.clone();
                                let nest_vocabulary_bounds = fields.serialize.vocabulary_bounds;
                                let nest_body = fields.serialize.body;

                                serialize_bounds.extend(fields.serialize.bounds);
                                serialize_vocabulary_bounds.add(fields.serialize.vocabulary_bounds);

                                let term = match fields.id_field {
                                    Some((field_access, ty)) => {
                                        serialize_bounds.push(quote! {
                                            #ty: ::serde_ld::LexicalRepresentation<V, I>
                                        });
                    
                                        quote! {
                                            #field_access.lexical_representation(interpretation, vocabulary, generator)
                                        }
                                    }
                                    None => quote! {
                                        ::serde_ld::Anonymous.lexical_representation(vocabulary, generator)
                                    },
                                };

                                quote! {
                                    struct _Nest<'_nest> #borrowed_fields;

                                    impl<'_nest, V, I> ::serde_ld::LexicalRepresentation<V, I> for _Nest<'_nest>
                                    where
                                        V: #nest_vocabulary_bounds,
                                        #(#nest_bounds),*
                                    {
                                        fn lexical_representation(
                                            &self,
                                            interpretation: &mut I,
                                            vocabulary: &mut V,
                                            generator: &mut impl ::serde_ld::rdf_types::Generator<V>,
                                        ) -> ::serde_ld::rdf_types::Term<::serde_ld::rdf_types::Id<V::Iri, V::BlankId>, V::Literal> {
                                            let _Nest #input = self;
                                            #term
                                        }
                                    }

                                    impl<'_nest, V, I> ::serde_ld::SerializeSubject<V, I> for _Nest<'_nest>
                                    where
                                        V: #nest_vocabulary_bounds,
                                        #(#nest_bounds),*
                                    {
                                        fn serialize_subject<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
                                        where
                                            S: ::serde_ld::SubjectSerializer<V, I>
                                        {
                                            let _Nest #input = self;
                                            #nest_body
                                        }
                                    }

                                    impl<'_nest, V, I> ::serde_ld::SerializePredicate<V, I> for _Nest<'_nest>
                                    where
                                        V: #nest_vocabulary_bounds,
                                        #(#nest_bounds),*
                                    {
                                        fn serialize_predicate<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
                                        where
                                            S: ::serde_ld::PredicateSerializer<V, I>
                                        {
                                            serializer.insert(self)?;
                                            serializer.end()
                                        }
                                    }

                                    serializer.insert(
                                        ::serde_ld::iref::Iri::new(#iri).unwrap(),
                                        &_Nest #input
                                    )?;
                                    
                                    serializer.end()
                                }
                            }
                            VariantShape::Unit => {
                                quote! {
                                    serializer.end()
                                }
                            }
                        }
                    }
                    None => {
                        if nested {
                            return Err(Error::MissingVariantIri(span))
                        }

                        match shape {
                            VariantShape::Simple(id, ty) => {
                                serialize_bounds.push(quote! {
                                    #ty: ::serde_ld::SerializeSubject<V, I>
                                });

                                quote! {
                                    <#ty as ::serde_ld::SerializeSubject<V, I>>::serialize_subject(#id, serializer)
                                }
                            }
                            VariantShape::Compound => {
                                let fields = fields(
                                    &attrs,
                                    variant.fields.clone(), 
                                    |f| quote!(#f), 
                                    |i| {
                                        let ident = format_ident!("a{i}");
                                        quote!(#ident)
                                    },
                                    |t| t
                                )?;
                                
                                serialize_bounds.extend(fields.serialize.bounds);
                                serialize_vocabulary_bounds.add(fields.serialize.vocabulary_bounds);
        
                                fields.serialize.body
                            }
                            VariantShape::Unit => {
                                quote! {
                                    serializer.end()
                                }
                            }
                        }
                    }
                };

                serialize_cases.push(quote!{
                    Self::#variant_id #input => {
                        #case
                    }
                });

                last_variant_span = Some(span)
            }

            Ok(quote! {
                // impl<V, I> ::serde_ld::LexicalRepresentation<V, I> for #ident
                // where
                //     #(#lexical_repr_bounds),*
                // {
                //     fn lexical_representation(
                //         &self,
                //         interpretation: &mut I,
                //         vocabulary: &mut V,
                //         generator: &mut impl ::serde_ld::rdf_types::Generator<V>,
                //     ) -> ::serde_ld::rdf_types::Term<::serde_ld::rdf_types::Id<V::Iri, V::BlankId>, V::Literal> {
                //         //
                //     }
                // }

                impl<V, I> ::serde_ld::SerializeSubject<V, I> for #ident
                where
                    V: #serialize_vocabulary_bounds,
                    #(#serialize_bounds),*
                {
                    fn serialize_subject<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
                    where
                        S: ::serde_ld::SubjectSerializer<V, I>
                    {
                        match self {
                            #(#serialize_cases)*
                        }
                    }
                }

                // impl<V, I> ::serde_ld::SerializePredicate<V, I> for #ident
                // where
                //     V: #vocabulary_bounds,
                //     #(#bounds),*
                // {
                //     fn serialize_predicate<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
                //     where
                //         S: ::serde_ld::PredicateSerializer<V, I>
                //     {
                //         serializer.insert(self)?;
                //         serializer.end()
                //     }
                // }

                // impl<V, I> ::serde_ld::SerializeGraph<V, I> for #ident
                // where
                //     V: #vocabulary_bounds,
                //     #(#bounds),*
                // {
                //     fn serialize_graph<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
                //     where
                //         S: ::serde_ld::GraphSerializer<V, I>
                //     {
                //         serializer.insert(self)?;
                //         serializer.end()
                //     }
                // }

                // impl<V, I> ::serde_ld::SerializeLd<V, I> for #ident
                // where
                //     V: #vocabulary_bounds,
                //     #(#bounds),*
                // {
                //     fn serialize<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
                //     where
                //         S: ::serde_ld::Serializer<V, I>
                //     {
                //         serializer.insert_default(self)?;
                //         serializer.end()
                //     }
                // }
            })
        }
        syn::Data::Union(u) => Err(Error::UnionType(u.union_token.span())),
    }
}

pub enum VariantShape {
    Simple(Ident, syn::Type),
    Compound,
    Unit
}

fn variant_shape(
    fields: &syn::Fields
) -> VariantShape {
    match fields {
        syn::Fields::Named(_) => VariantShape::Compound,
        syn::Fields::Unnamed(fields) => {
            let mut fields = fields.unnamed.iter();

            match fields.next() {
                Some(field) => {
                    if fields.next().is_some() {
                        VariantShape::Compound
                    } else {
                        if field.attrs.iter().any(|attr| attr.path().is_ident("ld")) {
                            VariantShape::Compound
                        } else {
                            let accessor = match &field.ident {
                                Some(id) => id.clone(),
                                None => format_ident!("a0")
                            };

                            VariantShape::Simple(accessor, field.ty.clone())
                        }
                    }
                }
                None => {
                    VariantShape::Compound
                }
            }
        }
        syn::Fields::Unit => VariantShape::Unit
    }
}

fn variant_nest(
    attrs: &TypeAttributes,
    variant_attrs: &VariantAttributes
) -> Result<Option<IriBuf>, Error> {
    match &variant_attrs.iri {
        Some(compact_iri) => {
            let iri = compact_iri.expand(&attrs.prefixes)?;
            Ok(Some(iri))
        }
        None => Ok(None)
    }
}

#[derive(Default, Clone, Copy)]
pub struct VocabularyBounds {
    iri_mut: bool
}

impl VocabularyBounds {
    pub fn add(&mut self, other: Self) {
        self.iri_mut |= other.iri_mut
    }
}

impl ToTokens for VocabularyBounds {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(quote! {
            ::serde_ld::rdf_types::Vocabulary
        });

        if self.iri_mut {
            tokens.extend(quote! {
                + ::serde_ld::rdf_types::IriVocabularyMut
            })
        }
    }
}

#[derive(Default)]
pub struct FieldsSerialization {
    bounds: Vec<TokenStream>,
    vocabulary_bounds: VocabularyBounds,
    body: TokenStream
}

#[derive(Default)]
pub struct Fields {
    serialize: FieldsSerialization,
    id_field: Option<(TokenStream, syn::Type)>
}

fn fields(
    attrs: &TypeAttributes,
    fields: syn::Fields,
    named_accessor: impl Fn(Ident) -> TokenStream,
    unnamed_accessor: impl Fn(u32) -> TokenStream,
    by_ref: impl Fn(TokenStream) -> TokenStream
) -> Result<Fields, Error> {
    let mut id_field = None;
    let mut serialize = FieldsSerialization::default();

    let mut serialize_fields = Vec::new();

    for (i, field) in fields.into_iter().enumerate() {
        let span = field.span();
        let field_attrs = read_field_attributes(field.attrs)?;
        
        let field_access = match field.ident {
            Some(id) => named_accessor(id),
            None => unnamed_accessor(i as u32)
        };
        
        let ty = field.ty;

        if field_attrs.is_id {
            id_field = Some((field_access, ty));
            continue;
        }

        let field_ref = by_ref(field_access);
        let serialize_field = if field_attrs.flatten {
            serialize.bounds.push(quote!(
                #ty: ::serde_ld::SerializeSubject<V, I>
            ));

            quote! {
                <#ty as ::serde_ld::SerializeSubject<V, I>>::serialize_subject(#field_ref, &mut serializer)?;
            }
        } else {
            match field_attrs.iri {
                Some(compact_iri) => {
                    let iri = compact_iri.expand(&attrs.prefixes)?.into_string();
                    serialize.vocabulary_bounds.iri_mut = true;
                    serialize.bounds.push(quote!(
                        #ty: ::serde_ld::SerializePredicate<V, I>
                    ));

                    quote! {
                        serializer.insert(
                            ::serde_ld::iref::Iri::new(#iri).unwrap(),
                            #field_ref
                        )?;
                    }
                }
                None => return Err(Error::UnknownFieldSerializationMethod(span)),
            }
        };

        serialize_fields.push(serialize_field)
    }

    serialize.body = quote! {
        #(#serialize_fields)*
        serializer.end()
    };

    Ok(Fields {
        id_field,
        serialize
    })
}