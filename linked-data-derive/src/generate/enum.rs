use iref::{Iri, IriBuf};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use syn::spanned::Spanned;

use crate::generate::{read_variant_attributes, TypeAttributes, VariantAttributes};

use super::{variant_compound_fields, Error, VocabularyBounds};

pub fn generate(
	attrs: &TypeAttributes,
	ident: Ident,
	e: syn::DataEnum,
) -> Result<TokenStream, Error> {
	let mut lexical_repr_bounds = Vec::new();
	let mut lexical_repr_cases = Vec::new();

	let mut visit_subject_bounds = Vec::new();
	let mut visit_subject_vocabulary_bounds = VocabularyBounds::default();
	let mut visit_subject_cases = Vec::new();

	let mut visit_predicate_bounds = Vec::new();
	let mut visit_predicate_vocabulary_bounds = VocabularyBounds::default();
	let mut visit_predicate_cases = Vec::new();

	let mut visit_graph_bounds = Vec::new();
	let mut visit_graph_vocabulary_bounds = VocabularyBounds::default();
	let mut visit_graph_cases = Vec::new();

	let mut visit_ld_bounds = Vec::new();
	let mut visit_ld_vocabulary_bounds = VocabularyBounds::default();
	let mut visit_ld_cases = Vec::new();

	// let mut visit_objects;

	let mut nested = false;
	let mut last_variant_span = None;
	let mut compound_types = Vec::new();

	for variant in e.variants {
		let span = variant.span();
		let variant_attrs = read_variant_attributes(variant.attrs)?;
		let variant = StrippedVariant::new(variant.ident, variant.fields);

		let nest = variant_nest(attrs, &variant_attrs)?;
		let shape = variant_shape(attrs, &ident, &variant)?;

		if nest.is_some() {
			if !nested {
				if let Some(span) = last_variant_span {
					return Err(Error::MissingVariantIri(span));
				}

				nested = true;
			}
		} else if nested {
			return Err(Error::MissingVariantIri(span));
		}

		let variant_id = &variant.ident;
		let input = &variant.input;

		let lexical_repr_case = variant_lexical_representation(
			&variant,
			nest.as_deref(),
			&shape,
			&mut lexical_repr_bounds,
		);

		lexical_repr_cases.push(quote! {
			Self::#variant_id #input => {
				#lexical_repr_case
			}
		});

		let visit_subject_case = variant_visit_subject(
			&variant,
			nest.as_deref(),
			&shape,
			&mut visit_subject_bounds,
			&mut visit_subject_vocabulary_bounds,
		);

		visit_subject_cases.push(quote! {
			Self::#variant_id #input => {
				#visit_subject_case
			}
		});

		let visit_predicate_case = variant_visit_predicate(
			&variant,
			nest.as_deref(),
			&shape,
			&mut visit_predicate_bounds,
			&mut visit_predicate_vocabulary_bounds,
		);

		visit_predicate_cases.push(quote! {
			Self::#variant_id #input => {
				#visit_predicate_case
			}
		});

		let visit_graph_case = variant_visit_graph(
			&variant,
			nest.as_deref(),
			&shape,
			&mut visit_graph_bounds,
			&mut visit_graph_vocabulary_bounds,
		);

		visit_graph_cases.push(quote! {
			Self::#variant_id #input => {
				#visit_graph_case
			}
		});

		let visit_ld_case = variant_serialize(
			&variant,
			nest.as_deref(),
			&shape,
			&mut visit_ld_bounds,
			&mut visit_ld_vocabulary_bounds,
		);

		visit_ld_cases.push(quote! {
			Self::#variant_id #input => {
				#visit_ld_case
			}
		});

		if let VariantShape::Compound(compound_type) = shape {
			compound_types.push(compound_type.definition)
		}

		last_variant_span = Some(span)
	}

	Ok(quote! {
		#(#compound_types)*

		impl<V, I> ::linked_data::LexicalRepresentation<V, I> for #ident
		where
			V: ::linked_data::rdf_types::Vocabulary,
			#(#lexical_repr_bounds),*
		{
			fn lexical_representation(
				&self,
				interpretation: &mut I,
				vocabulary: &mut V
			) -> Option<::linked_data::RdfTerm<V>> {
				match self {
					#(#lexical_repr_cases)*
				}
			}
		}

		impl<V, I> ::linked_data::LinkedDataSubject<V, I> for #ident
		where
			V: #visit_subject_vocabulary_bounds,
			#(#visit_subject_bounds),*
		{
			fn visit_subject<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
			where
				S: ::linked_data::SubjectVisitor<V, I>
			{
				match self {
					#(#visit_subject_cases)*
				}
			}
		}

		impl<V, I> ::linked_data::LinkedDataPredicateObjects<V, I> for #ident
		where
			V: #visit_predicate_vocabulary_bounds,
			#(#visit_predicate_bounds),*
		{
			fn visit_objects<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
			where
				S: ::linked_data::PredicateObjectsVisitor<V, I>
			{
				match self {
					#(#visit_predicate_cases)*
				}
			}
		}

		impl<V, I> ::linked_data::LinkedDataGraph<V, I> for #ident
		where
			V: #visit_graph_vocabulary_bounds,
			#(#visit_graph_bounds),*
		{
			fn visit_graph<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
			where
				S: ::linked_data::GraphVisitor<V, I>
			{
				match self {
					#(#visit_graph_cases)*
				}
			}
		}

		impl<V, I> ::linked_data::LinkedData<V, I> for #ident
		where
			V: #visit_ld_vocabulary_bounds,
			#(#visit_ld_bounds),*
		{
			fn visit<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
			where
				S: ::linked_data::Visitor<V, I>
			{
				match self {
					#(#visit_ld_cases)*
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
	pub fn new(ident: Ident, fields: syn::Fields) -> Self {
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
			syn::Fields::Unit => TokenStream::new(),
		};

		Self {
			ident,
			fields,
			input,
		}
	}
}

fn variant_lexical_representation(
	variant: &StrippedVariant,
	nest: Option<&Iri>,
	shape: &VariantShape,
	bounds: &mut Vec<TokenStream>,
) -> TokenStream {
	if nest.is_some() {
		match shape {
			VariantShape::Simple(_, _) => {
				quote! {
					None
				}
			}
			VariantShape::Compound(_inner_ty) => {
				quote! {
					None
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
					#ty: ::linked_data::LexicalRepresentation<V, I>
				});

				quote! {
					<#ty as ::linked_data::LexicalRepresentation<V, I>>::lexical_representation(
						#id,
						interpretation,
						vocabulary
					)
				}
			}
			VariantShape::Compound(inner_ty) => {
				let inner_id = &inner_ty.ident;
				let input = &variant.input;

				quote! {
					#inner_id #input .lexical_representation(interpretation, vocabulary)
				}
			}
			VariantShape::Unit => {
				quote! {
					None
				}
			}
		}
	}
}

fn variant_visit_subject(
	variant: &StrippedVariant,
	nest: Option<&Iri>,
	shape: &VariantShape,
	bounds: &mut Vec<TokenStream>,
	vocabulary_bounds: &mut VocabularyBounds,
) -> TokenStream {
	match nest {
		Some(iri) => {
			vocabulary_bounds.iri_mut = true;
			let iri = iri.as_str();

			match shape {
				VariantShape::Simple(id, ty) => {
					bounds.push(quote! {
						#ty: ::linked_data::LinkedDataPredicateObjects<V, I>
					});

					quote! {
						visitor.predicate(
							::linked_data::iref::Iri::new(#iri).unwrap(),
							#id
						)?;
						visitor.end()
					}
				}
				VariantShape::Compound(inner_ty) => {
					let inner_id = &inner_ty.ident;
					let input = &variant.input;

					bounds.extend(inner_ty.visit_bounds.iter().cloned());
					vocabulary_bounds.add(inner_ty.visit_vocabulary_bounds);

					quote! {
						visitor.predicate(
							::linked_data::iref::Iri::new(#iri).unwrap(),
							&#inner_id #input
						)?;

						visitor.end()
					}
				}
				VariantShape::Unit => {
					quote! {
						visitor.end()
					}
				}
			}
		}
		None => match shape {
			VariantShape::Simple(id, ty) => {
				bounds.push(quote! {
					#ty: ::linked_data::LinkedDataSubject<V, I>
				});

				quote! {
					<#ty as ::linked_data::LinkedDataSubject<V, I>>::visit_subject(#id, visitor)
				}
			}
			VariantShape::Compound(inner_ty) => {
				let inner_id = &inner_ty.ident;
				let input = &variant.input;

				bounds.extend(inner_ty.visit_bounds.iter().cloned());
				vocabulary_bounds.add(inner_ty.visit_vocabulary_bounds);

				quote! {
					#inner_id #input .visit_subject(visitor)
				}
			}
			VariantShape::Unit => {
				quote! {
					visitor.end()
				}
			}
		},
	}
}

fn variant_visit_predicate(
	variant: &StrippedVariant,
	nest: Option<&Iri>,
	shape: &VariantShape,
	bounds: &mut Vec<TokenStream>,
	vocabulary_bounds: &mut VocabularyBounds,
) -> TokenStream {
	match nest {
		Some(iri) => {
			let iri = iri.as_str();
			vocabulary_bounds.iri_mut = true;

			match shape {
				VariantShape::Simple(id, ty) => {
					bounds.push(quote! {
						#ty: ::linked_data::LinkedDataPredicateObjects<V, I> + ::linked_data::LexicalRepresentation<V, I>
					});

					quote! {
						::linked_data::AnonymousBinding(
							::linked_data::iref::Iri::new(#iri).unwrap(),
							#id
						).visit_objects(visitor)
					}
				}
				VariantShape::Compound(inner_ty) => {
					let inner_id = &inner_ty.ident;
					let input = &variant.input;

					bounds.extend(inner_ty.visit_bounds.iter().cloned());
					vocabulary_bounds.add(inner_ty.visit_vocabulary_bounds);

					quote! {
						::linked_data::AnonymousBinding(
							::linked_data::iref::Iri::new(#iri).unwrap(),
							&#inner_id #input
						).visit_objects(visitor)
					}
				}
				VariantShape::Unit => {
					quote! {
						visitor.object(::linked_data::iref::Iri::new(#iri).unwrap())?;
						visitor.end()
					}
				}
			}
		}
		None => match shape {
			VariantShape::Simple(id, ty) => {
				bounds.push(quote! {
					#ty: ::linked_data::LinkedDataPredicateObjects<V, I>
				});

				quote! {
					<#ty as ::linked_data::LinkedDataPredicateObjects<V, I>>::visit_objects(#id, visitor)
				}
			}
			VariantShape::Compound(inner_ty) => {
				let inner_id = &inner_ty.ident;
				let input = &variant.input;

				bounds.extend(inner_ty.visit_bounds.iter().cloned());
				vocabulary_bounds.add(inner_ty.visit_vocabulary_bounds);

				quote! {
					#inner_id #input .visit_objects(visitor)
				}
			}
			VariantShape::Unit => {
				quote! {
					visitor.end()
				}
			}
		},
	}
}

fn variant_visit_graph(
	variant: &StrippedVariant,
	nest: Option<&Iri>,
	shape: &VariantShape,
	bounds: &mut Vec<TokenStream>,
	vocabulary_bounds: &mut VocabularyBounds,
) -> TokenStream {
	match nest {
		Some(iri) => {
			let iri = iri.as_str();
			vocabulary_bounds.iri_mut = true;

			match shape {
				VariantShape::Simple(id, ty) => {
					bounds.push(quote! {
						#ty: ::linked_data::LinkedDataPredicateObjects<V, I> + ::linked_data::LexicalRepresentation<V, I>
					});

					quote! {
						::linked_data::AnonymousBinding(
							::linked_data::iref::Iri::new(#iri).unwrap(),
							#id
						).visit_graph(visitor)
					}
				}
				VariantShape::Compound(inner_ty) => {
					let inner_id = &inner_ty.ident;
					let input = &variant.input;

					bounds.extend(inner_ty.visit_bounds.iter().cloned());
					vocabulary_bounds.add(inner_ty.visit_vocabulary_bounds);

					quote! {
						::linked_data::AnonymousBinding(
							::linked_data::iref::Iri::new(#iri).unwrap(),
							&#inner_id #input
						).visit_graph(visitor)
					}
				}
				VariantShape::Unit => {
					quote! {
						visitor.subject(::linked_data::iref::Iri::new(#iri).unwrap())?;
						visitor.end()
					}
				}
			}
		}
		None => match shape {
			VariantShape::Simple(id, ty) => {
				bounds.push(quote! {
					#ty: ::linked_data::LinkedDataGraph<V, I>
				});

				quote! {
					<#ty as ::linked_data::LinkedDataGraph<V, I>>::visit_graph(#id, visitor)
				}
			}
			VariantShape::Compound(inner_ty) => {
				let inner_id = &inner_ty.ident;
				let input = &variant.input;

				bounds.extend(inner_ty.visit_bounds.iter().cloned());
				vocabulary_bounds.add(inner_ty.visit_vocabulary_bounds);

				quote! {
					#inner_id #input .visit_graph(visitor)
				}
			}
			VariantShape::Unit => {
				quote! {
					visitor.end()
				}
			}
		},
	}
}

fn variant_serialize(
	variant: &StrippedVariant,
	nest: Option<&Iri>,
	shape: &VariantShape,
	bounds: &mut Vec<TokenStream>,
	vocabulary_bounds: &mut VocabularyBounds,
) -> TokenStream {
	match nest {
		Some(iri) => {
			let iri = iri.as_str();
			vocabulary_bounds.iri_mut = true;

			match shape {
				VariantShape::Simple(id, ty) => {
					bounds.push(quote! {
						#ty: ::linked_data::LinkedDataPredicateObjects<V, I> + ::linked_data::LexicalRepresentation<V, I>
					});

					quote! {
						::linked_data::AnonymousBinding(
							::linked_data::iref::Iri::new(#iri).unwrap(),
							#id
						).visit(visitor)
					}
				}
				VariantShape::Compound(inner_ty) => {
					let inner_id = &inner_ty.ident;
					let input = &variant.input;

					bounds.extend(inner_ty.visit_bounds.iter().cloned());
					vocabulary_bounds.add(inner_ty.visit_vocabulary_bounds);

					quote! {
						::linked_data::AnonymousBinding(
							::linked_data::iref::Iri::new(#iri).unwrap(),
							&#inner_id #input
						).visit(visitor)
					}
				}
				VariantShape::Unit => {
					quote! {
						visitor.default_graph(::linked_data::iref::Iri::new(#iri).unwrap())?;
						visitor.end()
					}
				}
			}
		}
		None => match shape {
			VariantShape::Simple(id, ty) => {
				bounds.push(quote! {
					#ty: ::linked_data::LinkedData<V, I>
				});

				quote! {
					<#ty as ::linked_data::LinkedData<V, I>>::visit(#id, visitor)
				}
			}
			VariantShape::Compound(inner_ty) => {
				let inner_id = &inner_ty.ident;
				let input = &variant.input;

				bounds.extend(inner_ty.visit_bounds.iter().cloned());
				vocabulary_bounds.add(inner_ty.visit_vocabulary_bounds);

				quote! {
					#inner_id #input .visit(visitor)
				}
			}
			VariantShape::Unit => {
				quote! {
					visitor.end()
				}
			}
		},
	}
}

struct VariantSubjectType {
	ident: Ident,
	// lexical_repr_bounds: Vec<TokenStream>,
	visit_bounds: Vec<TokenStream>,
	visit_vocabulary_bounds: VocabularyBounds,
	definition: TokenStream,
}

fn variant_subject_type(
	attrs: &TypeAttributes,
	ident: &Ident,
	variant: &StrippedVariant,
) -> Result<VariantSubjectType, Error> {
	let compound_fields = variant_compound_fields(
		attrs,
		variant.fields.clone(),
		|f| quote!(#f),
		|i| {
			let ident = format_ident!("a{i}");
			quote!(#ident)
		},
		|t| t,
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
		syn::Fields::Unit => quote!(),
	};

	let mut lexical_repr_bounds = Vec::new();
	let visit_bounds = compound_fields.visit.bounds;
	let visit_vocabulary_bounds = compound_fields.visit.vocabulary_bounds;
	let visit_body = &compound_fields.visit.body;

	let term = match compound_fields.id_field {
		Some((field_access, ty)) => {
			// bounds.push(quote! {
			// 	#ty: ::linked_data::LexicalRepresentation<V, I>
			// });

			lexical_repr_bounds.push(quote! {
				#ty: ::linked_data::LexicalRepresentation<V, I>
			});

			quote! {
				#field_access.lexical_representation(interpretation, vocabulary)
			}
		}
		None => quote! {
			None
		},
	};

	let subject_id = format_ident!("_{ident}_{}", variant.ident);
	let input = &variant.input;

	let definition = quote! {
		#[allow(non_camel_case_types)]
		struct #subject_id<'_nest> #borrowed_fields;

		impl<'_nest, V, I> ::linked_data::LexicalRepresentation<V, I> for #subject_id<'_nest>
		where
			V: ::linked_data::rdf_types::Vocabulary,
			#(#lexical_repr_bounds),*
		{
			fn lexical_representation(
				&self,
				interpretation: &mut I,
				vocabulary: &mut V
			) -> Option<::linked_data::RdfTerm<V>> {
				let #subject_id #input = self;
				#term
			}
		}

		impl<'_nest, V, I> ::linked_data::LinkedDataSubject<V, I> for #subject_id<'_nest>
		where
			V: #visit_vocabulary_bounds,
			#(#visit_bounds),*
		{
			fn visit_subject<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
			where
				S: ::linked_data::SubjectVisitor<V, I>
			{
				let #subject_id #input = self;
				#visit_body
			}
		}

		impl<'_nest, V, I> ::linked_data::LinkedDataPredicateObjects<V, I> for #subject_id<'_nest>
		where
			V: #visit_vocabulary_bounds,
			#(#visit_bounds),*
		{
			fn visit_objects<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
			where
				S: ::linked_data::PredicateObjectsVisitor<V, I>
			{
				visitor.object(self)?;
				visitor.end()
			}
		}

		impl<'_nest, V, I> ::linked_data::LinkedDataGraph<V, I> for #subject_id<'_nest>
		where
			V: #visit_vocabulary_bounds,
			#(#visit_bounds),*
		{
			fn visit_graph<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
			where
				S: ::linked_data::GraphVisitor<V, I>
			{
				visitor.subject(self)?;
				visitor.end()
			}
		}

		impl<'_nest, V, I> ::linked_data::LinkedData<V, I> for #subject_id<'_nest>
		where
			V: #visit_vocabulary_bounds,
			#(#visit_bounds),*
		{
			fn visit<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
			where
				S: ::linked_data::Visitor<V, I>
			{
				visitor.default_graph(self)?;
				visitor.end()
			}
		}
	};

	Ok(VariantSubjectType {
		ident: subject_id,
		// lexical_repr_bounds,
		visit_bounds,
		visit_vocabulary_bounds,
		definition,
	})
}

enum VariantShape {
	Simple(Ident, syn::Type),
	Compound(VariantSubjectType),
	Unit,
}

fn variant_shape(
	attrs: &TypeAttributes,
	ident: &Ident,
	variant: &StrippedVariant,
) -> Result<VariantShape, Error> {
	match &variant.fields {
		syn::Fields::Named(_) => Ok(VariantShape::Compound(variant_subject_type(
			attrs, ident, variant,
		)?)),
		syn::Fields::Unnamed(unnamed_fields) => {
			let mut fields_iter = unnamed_fields.unnamed.iter();

			if let Some(field) = fields_iter.next() {
				if fields_iter.next().is_none()
					&& !field.attrs.iter().any(|attr| attr.path().is_ident("ld"))
				{
					let accessor = match &field.ident {
						Some(id) => id.clone(),
						None => format_ident!("a0"),
					};

					return Ok(VariantShape::Simple(accessor, field.ty.clone()));
				}
			}

			Ok(VariantShape::Compound(variant_subject_type(
				attrs, ident, variant,
			)?))
		}
		syn::Fields::Unit => Ok(VariantShape::Unit),
	}
}

fn variant_nest(
	attrs: &TypeAttributes,
	variant_attrs: &VariantAttributes,
) -> Result<Option<IriBuf>, Error> {
	match &variant_attrs.iri {
		Some(compact_iri) => {
			let iri = compact_iri.expand(&attrs.prefixes)?;
			Ok(Some(iri))
		}
		None => Ok(None),
	}
}
