use iref::{IriBuf, Iri};
use proc_macro2::{TokenStream, Ident};
use quote::{quote, format_ident};
use syn::spanned::Spanned;

use crate::generate::{TypeAttributes, VariantAttributes, read_variant_attributes};

use super::{Error, variant_compound_fields, VocabularyBounds};

pub fn generate(
	attrs: &TypeAttributes,
	ident: Ident,
	e: syn::DataEnum
) -> Result<TokenStream, Error> {
	let mut lexical_repr_bounds = Vec::new();
	let mut lexical_repr_cases = Vec::new();

	let mut serialize_subject_bounds = Vec::new();
	let mut serialize_subject_vocabulary_bounds = VocabularyBounds::default();
	let mut serialize_subject_cases = Vec::new();

	let mut serialize_predicate_bounds = Vec::new();
	let mut serialize_predicate_vocabulary_bounds = VocabularyBounds::default();
	let mut serialize_predicate_cases = Vec::new();

	let mut serialize_graph_bounds = Vec::new();
	let mut serialize_graph_vocabulary_bounds = VocabularyBounds::default();
	let mut serialize_graph_cases = Vec::new();

	let mut serialize_ld_bounds = Vec::new();
	let mut serialize_ld_vocabulary_bounds = VocabularyBounds::default();
	let mut serialize_ld_cases = Vec::new();

	// let mut serialize_predicate;

	let mut nested = false;
	let mut last_variant_span = None;
	let mut compound_types = Vec::new();

	for variant in e.variants {
		let span = variant.span();
		let variant_attrs = read_variant_attributes(variant.attrs)?;
		let variant = StrippedVariant::new(
			variant.ident,
			variant.fields
		);

		let nest = variant_nest(&attrs, &variant_attrs)?;
		let shape = variant_shape(&attrs, &ident, &variant)?;
		
		if nest.is_some() {
			if !nested {
				if let Some(span) = last_variant_span {
					return Err(Error::MissingVariantIri(span))
				}

				nested = true;
			}
		} else {
			if nested {
				return Err(Error::MissingVariantIri(span))
			}
		}

		let variant_id = &variant.ident;
		let input = &variant.input;

		let lexical_repr_case = variant_lexical_representation(
			&variant,
			nest.as_deref(),
			&shape,
			&mut lexical_repr_bounds
		);

		lexical_repr_cases.push(quote!{
			Self::#variant_id #input => {
				#lexical_repr_case
			}
		});

		let serialize_subject_case = variant_serialize_subject(
			&variant,
			nest.as_deref(),
			&shape,
			&mut serialize_subject_bounds,
			&mut serialize_subject_vocabulary_bounds
		);

		serialize_subject_cases.push(quote!{
			Self::#variant_id #input => {
				#serialize_subject_case
			}
		});

		let serialize_predicate_case = variant_serialize_predicate(
			&variant,
			nest.as_deref(),
			&shape,
			&mut serialize_predicate_bounds,
			&mut serialize_predicate_vocabulary_bounds
		);

		serialize_predicate_cases.push(quote!{
			Self::#variant_id #input => {
				#serialize_predicate_case
			}
		});

		let serialize_graph_case = variant_serialize_graph(
			&variant,
			nest.as_deref(),
			&shape,
			&mut serialize_graph_bounds,
			&mut serialize_graph_vocabulary_bounds
		);

		serialize_graph_cases.push(quote!{
			Self::#variant_id #input => {
				#serialize_graph_case
			}
		});

		let serialize_ld_case = variant_serialize(
			&variant,
			nest.as_deref(),
			&shape,
			&mut serialize_ld_bounds,
			&mut serialize_ld_vocabulary_bounds
		);

		serialize_ld_cases.push(quote!{
			Self::#variant_id #input => {
				#serialize_ld_case
			}
		});

		if let VariantShape::Compound(compound_type) = shape {
			compound_types.push(compound_type.definition)
		}

		last_variant_span = Some(span)
	}

	Ok(quote! {
		#(#compound_types)*

		impl<V, I> ::serde_ld::LexicalRepresentation<V, I> for #ident
		where
			V: ::serde_ld::rdf_types::Vocabulary,
			#(#lexical_repr_bounds),*
		{
			fn lexical_representation(
				&self,
				interpretation: &mut I,
				vocabulary: &mut V,
				generator: &mut impl ::serde_ld::rdf_types::Generator<V>,
			) -> ::serde_ld::rdf_types::Term<::serde_ld::rdf_types::Id<V::Iri, V::BlankId>, V::Literal> {
				match self {
					#(#lexical_repr_cases)*
				}
			}
		}

		impl<V, I> ::serde_ld::SerializeSubject<V, I> for #ident
		where
			V: #serialize_subject_vocabulary_bounds,
			#(#serialize_subject_bounds),*
		{
			fn serialize_subject<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
			where
				S: ::serde_ld::SubjectSerializer<V, I>
			{
				match self {
					#(#serialize_subject_cases)*
				}
			}
		}

		impl<V, I> ::serde_ld::SerializePredicate<V, I> for #ident
		where
			V: #serialize_predicate_vocabulary_bounds,
			#(#serialize_predicate_bounds),*
		{
			fn serialize_predicate<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
			where
				S: ::serde_ld::PredicateSerializer<V, I>
			{
				match self {
					#(#serialize_predicate_cases)*
				}
			}
		}

		impl<V, I> ::serde_ld::SerializeGraph<V, I> for #ident
		where
			V: #serialize_graph_vocabulary_bounds,
			#(#serialize_graph_bounds),*
		{
		    fn serialize_graph<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
		    where
		        S: ::serde_ld::GraphSerializer<V, I>
		    {
		        match self {
					#(#serialize_graph_cases)*
				}
		    }
		}

		impl<V, I> ::serde_ld::SerializeLd<V, I> for #ident
		where
			V: #serialize_ld_vocabulary_bounds,
			#(#serialize_ld_bounds),*
		{
		    fn serialize<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
		    where
		        S: ::serde_ld::Serializer<V, I>
		    {
		        match self {
					#(#serialize_ld_cases)*
				}
		    }
		}
	})
}

struct StrippedVariant {
	ident: Ident,
	fields: syn::Fields,
	input: TokenStream,
}

impl StrippedVariant {
	pub fn new(
		ident: Ident,
		fields: syn::Fields,
	) -> Self {
		let input = match &fields {
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

		Self {
			ident,
			fields,
			input
		}
	}
}

fn variant_lexical_representation(
	variant: &StrippedVariant,
	nest: Option<&Iri>,
	shape: &VariantShape,
	bounds: &mut Vec<TokenStream>
) -> TokenStream {
	if nest.is_some() {
		match shape {
			VariantShape::Simple(_, _) => {
				quote! {
					::serde_ld::Anonymous.lexical_representation(
						vocabulary,
						generator
					)
				}
			}
			VariantShape::Compound(_inner_ty) => {
				quote! {
					::serde_ld::Anonymous.lexical_representation(
						vocabulary,
						generator
					)
				}
			}
			VariantShape::Unit => {
				quote! {
					todo!()
				}
			}
		}
	} else {
		match shape {
			VariantShape::Simple(id, ty) => {
				bounds.push(quote! {
					#ty: ::serde_ld::LexicalRepresentation<V, I>
				});

				quote! {
					<#ty as ::serde_ld::LexicalRepresentation<V, I>>::lexical_representation(
						#id,
						interpretation,
						vocabulary,
						generator
					)
				}
			}
			VariantShape::Compound(inner_ty) => {
				let inner_id = &inner_ty.ident;
				let input = &variant.input;

				quote! {
					#inner_id #input .lexical_representation(interpretation, vocabulary, generator)
				}
			}
			VariantShape::Unit => {
				quote! {
					::serde_ld::Anonymous.lexical_representation(vocabulary, generator)
				}
			}
		}
	}
}

fn variant_serialize_subject(
	variant: &StrippedVariant,
	nest: Option<&Iri>,
	shape: &VariantShape,
	bounds: &mut Vec<TokenStream>,
	vocabulary_bounds: &mut VocabularyBounds
) -> TokenStream {
	match nest {
		Some(iri) => {
			vocabulary_bounds.iri_mut = true;
			let iri = iri.as_str();

			match shape {
				VariantShape::Simple(id, ty) => {
					bounds.push(quote! {
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
				VariantShape::Compound(inner_ty) => {
					let inner_id = &inner_ty.ident;
					let input = &variant.input;

					bounds.extend(inner_ty.serialize_bounds.iter().cloned());
					vocabulary_bounds.add(inner_ty.serialize_vocabulary_bounds);

					quote! {
						serializer.insert(
							::serde_ld::iref::Iri::new(#iri).unwrap(),
							&#inner_id #input
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
			match shape {
				VariantShape::Simple(id, ty) => {
					bounds.push(quote! {
						#ty: ::serde_ld::SerializeSubject<V, I>
					});

					quote! {
						<#ty as ::serde_ld::SerializeSubject<V, I>>::serialize_subject(#id, serializer)
					}
				}
				VariantShape::Compound(inner_ty) => {
					let inner_id = &inner_ty.ident;
					let input = &variant.input;

					bounds.extend(inner_ty.serialize_bounds.iter().cloned());
					vocabulary_bounds.add(inner_ty.serialize_vocabulary_bounds);

					quote! {
						#inner_id #input .serialize_subject(serializer)
					}
				}
				VariantShape::Unit => {
					quote! {
						serializer.end()
					}
				}
			}
		}
	}
}

fn variant_serialize_predicate(
	variant: &StrippedVariant,
	nest: Option<&Iri>,
	shape: &VariantShape,
	bounds: &mut Vec<TokenStream>,
	vocabulary_bounds: &mut VocabularyBounds
) -> TokenStream {
	match nest {
		Some(iri) => {
			let iri = iri.as_str();
			vocabulary_bounds.iri_mut = true;

			match shape {
				VariantShape::Simple(id, ty) => {
					bounds.push(quote! {
						#ty: ::serde_ld::SerializePredicate<V, I> + ::serde_ld::LexicalRepresentation<V, I>
					});

					quote! {
						::serde_ld::AnonymousBinding(
							::serde_ld::iref::Iri::new(#iri).unwrap(),
							#id
						).serialize_predicate(serializer)
					}
				}
				VariantShape::Compound(inner_ty) => {
					let inner_id = &inner_ty.ident;
					let input = &variant.input;

					bounds.extend(inner_ty.serialize_bounds.iter().cloned());
					vocabulary_bounds.add(inner_ty.serialize_vocabulary_bounds);

					quote! {
						::serde_ld::AnonymousBinding(
							::serde_ld::iref::Iri::new(#iri).unwrap(),
							&#inner_id #input
						).serialize_predicate(serializer)
					}
				}
				VariantShape::Unit => {
					quote! {
						serializer.insert(::serde_ld::iref::Iri::new(#iri).unwrap())?;
						serializer.end()
					}
				}
			}
		}
		None => {
			match shape {
				VariantShape::Simple(id, ty) => {
					bounds.push(quote! {
						#ty: ::serde_ld::SerializePredicate<V, I>
					});

					quote! {
						<#ty as ::serde_ld::SerializePredicate<V, I>>::serialize_predicate(#id, serializer)
					}
				}
				VariantShape::Compound(inner_ty) => {
					let inner_id = &inner_ty.ident;
					let input = &variant.input;

					bounds.extend(inner_ty.serialize_bounds.iter().cloned());
					vocabulary_bounds.add(inner_ty.serialize_vocabulary_bounds);

					quote! {
						#inner_id #input .serialize_predicate(serializer)
					}
				}
				VariantShape::Unit => {
					quote! {
						serializer.end()
					}
				}
			}
		}
	}
}

fn variant_serialize_graph(
	variant: &StrippedVariant,
	nest: Option<&Iri>,
	shape: &VariantShape,
	bounds: &mut Vec<TokenStream>,
	vocabulary_bounds: &mut VocabularyBounds
) -> TokenStream {
	match nest {
		Some(iri) => {
			let iri = iri.as_str();
			vocabulary_bounds.iri_mut = true;

			match shape {
				VariantShape::Simple(id, ty) => {
					bounds.push(quote! {
						#ty: ::serde_ld::SerializePredicate<V, I> + ::serde_ld::LexicalRepresentation<V, I>
					});

					quote! {
						::serde_ld::AnonymousBinding(
							::serde_ld::iref::Iri::new(#iri).unwrap(),
							#id
						).serialize_graph(serializer)
					}
				}
				VariantShape::Compound(inner_ty) => {
					let inner_id = &inner_ty.ident;
					let input = &variant.input;

					bounds.extend(inner_ty.serialize_bounds.iter().cloned());
					vocabulary_bounds.add(inner_ty.serialize_vocabulary_bounds);

					quote! {
						::serde_ld::AnonymousBinding(
							::serde_ld::iref::Iri::new(#iri).unwrap(),
							&#inner_id #input
						).serialize_graph(serializer)
					}
				}
				VariantShape::Unit => {
					quote! {
						serializer.insert(::serde_ld::iref::Iri::new(#iri).unwrap())?;
						serializer.end()
					}
				}
			}
		}
		None => {
			match shape {
				VariantShape::Simple(id, ty) => {
					bounds.push(quote! {
						#ty: ::serde_ld::SerializeGraph<V, I>
					});

					quote! {
						<#ty as ::serde_ld::SerializeGraph<V, I>>::serialize_graph(#id, serializer)
					}
				}
				VariantShape::Compound(inner_ty) => {
					let inner_id = &inner_ty.ident;
					let input = &variant.input;

					bounds.extend(inner_ty.serialize_bounds.iter().cloned());
					vocabulary_bounds.add(inner_ty.serialize_vocabulary_bounds);

					quote! {
						#inner_id #input .serialize_graph(serializer)
					}
				}
				VariantShape::Unit => {
					quote! {
						serializer.end()
					}
				}
			}
		}
	}
}

fn variant_serialize(
	variant: &StrippedVariant,
	nest: Option<&Iri>,
	shape: &VariantShape,
	bounds: &mut Vec<TokenStream>,
	vocabulary_bounds: &mut VocabularyBounds
) -> TokenStream {
	match nest {
		Some(iri) => {
			let iri = iri.as_str();
			vocabulary_bounds.iri_mut = true;

			match shape {
				VariantShape::Simple(id, ty) => {
					bounds.push(quote! {
						#ty: ::serde_ld::SerializePredicate<V, I> + ::serde_ld::LexicalRepresentation<V, I>
					});

					quote! {
						::serde_ld::AnonymousBinding(
							::serde_ld::iref::Iri::new(#iri).unwrap(),
							#id
						).serialize(serializer)
					}
				}
				VariantShape::Compound(inner_ty) => {
					let inner_id = &inner_ty.ident;
					let input = &variant.input;

					bounds.extend(inner_ty.serialize_bounds.iter().cloned());
					vocabulary_bounds.add(inner_ty.serialize_vocabulary_bounds);

					quote! {
						::serde_ld::AnonymousBinding(
							::serde_ld::iref::Iri::new(#iri).unwrap(),
							&#inner_id #input
						).serialize(serializer)
					}
				}
				VariantShape::Unit => {
					quote! {
						serializer.insert_default(::serde_ld::iref::Iri::new(#iri).unwrap())?;
						serializer.end()
					}
				}
			}
		}
		None => {
			match shape {
				VariantShape::Simple(id, ty) => {
					bounds.push(quote! {
						#ty: ::serde_ld::SerializeLd<V, I>
					});

					quote! {
						<#ty as ::serde_ld::SerializeLd<V, I>>::serialize(#id, serializer)
					}
				}
				VariantShape::Compound(inner_ty) => {
					let inner_id = &inner_ty.ident;
					let input = &variant.input;

					bounds.extend(inner_ty.serialize_bounds.iter().cloned());
					vocabulary_bounds.add(inner_ty.serialize_vocabulary_bounds);

					quote! {
						#inner_id #input .serialize(serializer)
					}
				}
				VariantShape::Unit => {
					quote! {
						serializer.end()
					}
				}
			}
		}
	}
}

struct VariantSubjectType {
	ident: Ident,
	// lexical_repr_bounds: Vec<TokenStream>,
	serialize_bounds: Vec<TokenStream>,
	serialize_vocabulary_bounds: VocabularyBounds,
	definition: TokenStream
}

fn variant_subject_type(
	attrs: &TypeAttributes,
	ident: &Ident,
	variant: &StrippedVariant
) -> Result<VariantSubjectType, Error> {
	let compound_fields = variant_compound_fields(
		attrs,
		variant.fields.clone(), 
		|f| quote!(#f), 
		|i| {
			let ident = format_ident!("a{i}");
			quote!(#ident)
		},
		|t| t
	)?;

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

	let mut lexical_repr_bounds = Vec::new();
	let serialize_bounds = compound_fields.serialize.bounds;
	let serialize_vocabulary_bounds = compound_fields.serialize.vocabulary_bounds;
	let serialize_body = &compound_fields.serialize.body;

	let term = match compound_fields.id_field {
		Some((field_access, ty)) => {
			// bounds.push(quote! {
			// 	#ty: ::serde_ld::LexicalRepresentation<V, I>
			// });

			lexical_repr_bounds.push(quote! {
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

	let subject_id = format_ident!("_{ident}_{}", variant.ident);
	let input = &variant.input;

	let definition = quote! {
		#[allow(non_camel_case_types)]
		struct #subject_id<'_nest> #borrowed_fields;

		impl<'_nest, V, I> ::serde_ld::LexicalRepresentation<V, I> for #subject_id<'_nest>
		where
			V: ::serde_ld::rdf_types::Vocabulary,
			#(#lexical_repr_bounds),*
		{
			fn lexical_representation(
				&self,
				interpretation: &mut I,
				vocabulary: &mut V,
				generator: &mut impl ::serde_ld::rdf_types::Generator<V>,
			) -> ::serde_ld::rdf_types::Term<::serde_ld::rdf_types::Id<V::Iri, V::BlankId>, V::Literal> {
				let #subject_id #input = self;
				#term
			}
		}

		impl<'_nest, V, I> ::serde_ld::SerializeSubject<V, I> for #subject_id<'_nest>
		where
			V: #serialize_vocabulary_bounds,
			#(#serialize_bounds),*
		{
			fn serialize_subject<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
			where
				S: ::serde_ld::SubjectSerializer<V, I>
			{
				let #subject_id #input = self;
				#serialize_body
			}
		}

		impl<'_nest, V, I> ::serde_ld::SerializePredicate<V, I> for #subject_id<'_nest>
		where
			V: #serialize_vocabulary_bounds,
			#(#serialize_bounds),*
		{
			fn serialize_predicate<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
			where
				S: ::serde_ld::PredicateSerializer<V, I>
			{
				serializer.insert(self)?;
				serializer.end()
			}
		}

		impl<'_nest, V, I> ::serde_ld::SerializeGraph<V, I> for #subject_id<'_nest>
		where
			V: #serialize_vocabulary_bounds,
			#(#serialize_bounds),*
		{
			fn serialize_graph<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
			where
				S: ::serde_ld::GraphSerializer<V, I>
			{
				serializer.insert(self)?;
				serializer.end()
			}
		}

		impl<'_nest, V, I> ::serde_ld::SerializeLd<V, I> for #subject_id<'_nest>
		where
			V: #serialize_vocabulary_bounds,
			#(#serialize_bounds),*
		{
			fn serialize<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
			where
				S: ::serde_ld::Serializer<V, I>
			{
				serializer.insert_default(self)?;
				serializer.end()
			}
		}
	};

	Ok(VariantSubjectType {
		ident: subject_id,
		// lexical_repr_bounds,
		serialize_bounds,
		serialize_vocabulary_bounds,
		definition
	})
}

enum VariantShape {
    Simple(Ident, syn::Type),
    Compound(VariantSubjectType),
    Unit
}

fn variant_shape(
    attrs: &TypeAttributes,
	ident: &Ident,
    variant: &StrippedVariant
) -> Result<VariantShape, Error> {
    match &variant.fields {
        syn::Fields::Named(_) => {
            Ok(VariantShape::Compound(variant_subject_type(attrs, ident, variant)?))
        },
        syn::Fields::Unnamed(unnamed_fields) => {
            let mut fields_iter = unnamed_fields.unnamed.iter();

            if let Some(field) = fields_iter.next() {
                if fields_iter.next().is_none() && !field.attrs.iter().any(|attr| attr.path().is_ident("ld")) {
                    let accessor = match &field.ident {
                        Some(id) => id.clone(),
                        None => format_ident!("a0")
                    };

                    return Ok(VariantShape::Simple(accessor, field.ty.clone()))
                }
            }

            Ok(VariantShape::Compound(variant_subject_type(attrs, ident, variant)?))
        }
        syn::Fields::Unit => Ok(VariantShape::Unit)
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