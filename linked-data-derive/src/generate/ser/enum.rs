use iref::{Iri, IriBuf};
use proc_macro2::{Ident, TokenStream, Span};
use quote::{format_ident, quote};
use syn::{spanned::Spanned, punctuated::Punctuated};

use crate::generate::{read_variant_attributes, TypeAttributes, VariantAttributes, extend_generics};

use super::{variant_compound_fields, Error, VocabularyBounds};

pub fn generate(
	attrs: &TypeAttributes,
	ident: Ident,
	generics: syn::Generics,
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
		let shape = variant_shape(attrs, &ident, &generics, &variant)?;

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

		let lexical_repr_case =
			variant_interpret(&variant, nest.as_deref(), &shape, &mut lexical_repr_bounds);

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

	lexical_repr_bounds.push(syn::parse2(quote! {
		V: ::linked_data::rdf_types::Vocabulary
	}).unwrap());
	lexical_repr_bounds.push(syn::parse2(quote! {
		I: ::linked_data::rdf_types::Interpretation
	}).unwrap());

	let repr_generics = extend_generics(&generics, VocabularyBounds::default(), lexical_repr_bounds);
	let subject_generics = extend_generics(&generics, visit_subject_vocabulary_bounds, visit_subject_bounds);
	let predicate_generics = extend_generics(&generics, visit_predicate_vocabulary_bounds, visit_predicate_bounds);
	let graph_generics = extend_generics(&generics, visit_graph_vocabulary_bounds, visit_graph_bounds);
	let dataset_generics = extend_generics(&generics, visit_ld_vocabulary_bounds, visit_ld_bounds);

	let (_, ty_generics, _) = generics.split_for_impl();
	let (repr_impl_generics, _, repr_where_clauses) = repr_generics.split_for_impl();
	let (subject_impl_generics, _, subject_where_clauses) = subject_generics.split_for_impl();
	let (predicate_impl_generics, _, predicate_where_clauses) = predicate_generics.split_for_impl();
	let (graph_impl_generics, _, graph_where_clauses) = graph_generics.split_for_impl();
	let (dataset_impl_generics, _, dataset_where_clauses) = dataset_generics.split_for_impl();

	Ok(quote! {
		#(#compound_types)*

		impl #repr_impl_generics ::linked_data::Interpret<V, I> for #ident #ty_generics #repr_where_clauses {
			fn interpret(
				&self,
				vocabulary: &mut V,
				interpretation: &mut I
			) -> linked_data::ResourceInterpretation<V, I> {
				match self {
					#(#lexical_repr_cases)*
				}
			}
		}

		impl #subject_impl_generics ::linked_data::LinkedDataSubject<V, I> for #ident #ty_generics #subject_where_clauses {
			fn visit_subject<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
			where
				S: ::linked_data::SubjectVisitor<V, I>
			{
				match self {
					#(#visit_subject_cases)*
				}
			}
		}

		impl #predicate_impl_generics ::linked_data::LinkedDataPredicateObjects<V, I> for #ident #ty_generics #predicate_where_clauses {
			fn visit_objects<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
			where
				S: ::linked_data::PredicateObjectsVisitor<V, I>
			{
				match self {
					#(#visit_predicate_cases)*
				}
			}
		}

		impl #graph_impl_generics ::linked_data::LinkedDataGraph<V, I> for #ident #ty_generics #graph_where_clauses {
			fn visit_graph<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
			where
				S: ::linked_data::GraphVisitor<V, I>
			{
				match self {
					#(#visit_graph_cases)*
				}
			}
		}

		impl #dataset_impl_generics ::linked_data::LinkedData<V, I> for #ident #ty_generics #dataset_where_clauses {
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

pub trait UsesGenericParam {
	fn uses_generic_param(&self, p: &syn::GenericParam) -> bool;
}

impl UsesGenericParam for StrippedVariant {
	fn uses_generic_param(&self, p: &syn::GenericParam) -> bool {
		self.fields.iter().any(|f| f.ty.uses_generic_param(p))
	}
}

impl UsesGenericParam for syn::Type {
	fn uses_generic_param(&self, p: &syn::GenericParam) -> bool {
		match self {
			Self::Array(a) => {
				a.elem.uses_generic_param(p)
			}
			Self::BareFn(t) => {
				t.inputs.iter().any(|i| i.ty.uses_generic_param(p)) || t.output.uses_generic_param(p)
			}
			Self::Group(g) => {
				g.elem.uses_generic_param(p)
			}
			Self::ImplTrait(t) => {
				t.bounds.iter().any(|b| b.uses_generic_param(p))
			}
			Self::Paren(t) => t.elem.uses_generic_param(p),
			Self::Path(t) => t.uses_generic_param(p),
			Self::Ptr(t) => t.elem.uses_generic_param(p),
			Self::Reference(t) => {
				if let Some(l) = &t.lifetime {
					if l.uses_generic_param(p) {
						return true
					}
				}

				t.elem.uses_generic_param(p)
			}
			Self::Slice(t) => t.elem.uses_generic_param(p),
			Self::TraitObject(t) => {
				t.bounds.iter().any(|b| b.uses_generic_param(p))
			}
			Self::Tuple(t) => {
				t.elems.iter().any(|t| t.uses_generic_param(p))
			}
			_ => false
		}
	}
}

impl UsesGenericParam for syn::TypePath {
	fn uses_generic_param(&self, p: &syn::GenericParam) -> bool {
		if let Some(qself) = &self.qself {
			if qself.ty.uses_generic_param(p) {
				return true
			}
		}

		self.path.uses_generic_param(p)
	}
}

impl UsesGenericParam for syn::ReturnType {
	fn uses_generic_param(&self, p: &syn::GenericParam) -> bool {
		match self {
			Self::Default => false,
			Self::Type(_, ty) => ty.uses_generic_param(p)
		}
	}
}

impl UsesGenericParam for syn::TypeParamBound {
	fn uses_generic_param(&self, p: &syn::GenericParam) -> bool {
		match self {
			Self::Lifetime(l) => l.uses_generic_param(p),
			Self::Trait(t) => t.uses_generic_param(p),
			_ => false
		}
	}
}

impl UsesGenericParam for syn::Lifetime {
	fn uses_generic_param(&self, p: &syn::GenericParam) -> bool {
		match p {
			syn::GenericParam::Lifetime(l) => self.ident == l.lifetime.ident,
			_ => false
		}
	}
}

impl UsesGenericParam for syn::TraitBound {
	fn uses_generic_param(&self, p: &syn::GenericParam) -> bool {
		self.path.uses_generic_param(p)
	}
}

impl UsesGenericParam for syn::Path {
	fn uses_generic_param(&self, p: &syn::GenericParam) -> bool {
		if let syn::GenericParam::Type(t) = p {
			if self.segments.len() == 1 && self.segments.first().unwrap().ident == t.ident {
				return true
			}
		}

		self.segments.iter().any(|s| s.uses_generic_param(p))
	}
}

impl UsesGenericParam for syn::PathSegment {
	fn uses_generic_param(&self, p: &syn::GenericParam) -> bool {
		match &self.arguments {
			syn::PathArguments::None => false,
			syn::PathArguments::AngleBracketed(a) => {
				a.args.iter().any(|a| a.uses_generic_param(p))
			}
			syn::PathArguments::Parenthesized(a) => {
				a.inputs.iter().any(|i| i.uses_generic_param(p)) || a.output.uses_generic_param(p)
			}
		}
	}
}

impl UsesGenericParam for syn::GenericArgument {
	fn uses_generic_param(&self, p: &syn::GenericParam) -> bool {
		match self {
			Self::AssocType(a) => a.uses_generic_param(p),
			Self::Constraint(c) => c.uses_generic_param(p),
			Self::Lifetime(l) => l.uses_generic_param(p),
			Self::Type(t) => t.uses_generic_param(p),
			_ => false
		}
	}
}

impl UsesGenericParam for syn::AssocType {
	fn uses_generic_param(&self, p: &syn::GenericParam) -> bool {
		if let Some(generics) = &self.generics {
			if generics.args.iter().any(|a| a.uses_generic_param(p)) {
				return true
			}
		}

		self.ty.uses_generic_param(p)
	}
}

impl UsesGenericParam for syn::Constraint {
	fn uses_generic_param(&self, p: &syn::GenericParam) -> bool {
		if let Some(generics) = &self.generics {
			if generics.args.iter().any(|a| a.uses_generic_param(p)) {
				return true
			}
		}

		self.bounds.iter().any(|b| b.uses_generic_param(p))
	}
}

fn variant_interpret(
	variant: &StrippedVariant,
	nest: Option<&Iri>,
	shape: &VariantShape,
	bounds: &mut Vec<syn::WherePredicate>,
) -> TokenStream {
	if nest.is_some() {
		match shape {
			VariantShape::Simple(_, _) => {
				quote! {
					::linked_data::ResourceInterpretation::Uninterpreted(None)
				}
			}
			VariantShape::Compound(inner_ty) => {
				bounds.extend(inner_ty.lexical_repr_bounds.iter().cloned());
				
				quote! {
					::linked_data::ResourceInterpretation::Uninterpreted(None)
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
				bounds.push(syn::parse2(quote! {
					#ty: ::linked_data::Interpret<V, I>
				}).unwrap());

				quote! {
					<#ty as ::linked_data::Interpret<V, I>>::interpret(
						#id,
						vocabulary,
						interpretation
					)
				}
			}
			VariantShape::Compound(inner_ty) => {
				bounds.extend(inner_ty.lexical_repr_bounds.iter().cloned());
				let inner_id = &inner_ty.ident;
				let input = &variant.input;

				quote! {
					::linked_data::InterpretRef::interpret_ref(&#inner_id #input, vocabulary, interpretation)
				}
			}
			VariantShape::Unit => {
				quote! {
					::linked_data::ResourceInterpretation::Uninterpreted(None)
				}
			}
		}
	}
}

fn variant_visit_subject(
	variant: &StrippedVariant,
	nest: Option<&Iri>,
	shape: &VariantShape,
	bounds: &mut Vec<syn::WherePredicate>,
	vocabulary_bounds: &mut VocabularyBounds,
) -> TokenStream {
	match nest {
		Some(iri) => {
			vocabulary_bounds.iri_mut = true;
			let iri = iri.as_str();

			match shape {
				VariantShape::Simple(id, ty) => {
					bounds.push(syn::parse2(quote! {
						#ty: ::linked_data::LinkedDataPredicateObjects<V, I>
					}).unwrap());

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
				bounds.push(syn::parse2(quote! {
					#ty: ::linked_data::LinkedDataSubject<V, I>
				}).unwrap());

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
	bounds: &mut Vec<syn::WherePredicate>,
	vocabulary_bounds: &mut VocabularyBounds,
) -> TokenStream {
	match nest {
		Some(iri) => {
			let iri = iri.as_str();
			vocabulary_bounds.iri_mut = true;

			match shape {
				VariantShape::Simple(id, ty) => {
					bounds.push(syn::parse2(quote! {
						#ty: ::linked_data::LinkedDataPredicateObjects<V, I> + ::linked_data::Interpret<V, I>
					}).unwrap());

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
				bounds.push(syn::parse2(quote! {
					#ty: ::linked_data::LinkedDataPredicateObjects<V, I>
				}).unwrap());

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
	bounds: &mut Vec<syn::WherePredicate>,
	vocabulary_bounds: &mut VocabularyBounds,
) -> TokenStream {
	match nest {
		Some(iri) => {
			let iri = iri.as_str();
			vocabulary_bounds.iri_mut = true;

			match shape {
				VariantShape::Simple(id, ty) => {
					bounds.push(syn::parse2(quote! {
						#ty: ::linked_data::LinkedDataPredicateObjects<V, I> + ::linked_data::Interpret<V, I>
					}).unwrap());

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
				bounds.push(syn::parse2(quote! {
					#ty: ::linked_data::LinkedDataGraph<V, I>
				}).unwrap());

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
	bounds: &mut Vec<syn::WherePredicate>,
	vocabulary_bounds: &mut VocabularyBounds,
) -> TokenStream {
	match nest {
		Some(iri) => {
			let iri = iri.as_str();
			vocabulary_bounds.iri_mut = true;

			match shape {
				VariantShape::Simple(id, ty) => {
					bounds.push(syn::parse2(quote! {
						#ty: ::linked_data::LinkedDataPredicateObjects<V, I> + ::linked_data::Interpret<V, I>
					}).unwrap());

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
				bounds.push(syn::parse2(quote! {
					#ty: ::linked_data::LinkedData<V, I>
				}).unwrap());

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
	lexical_repr_bounds: Vec<syn::WherePredicate>,
	visit_bounds: Vec<syn::WherePredicate>,
	visit_vocabulary_bounds: VocabularyBounds,
	definition: TokenStream,
}

fn variant_subject_type(
	attrs: &TypeAttributes,
	ident: &Ident,
	generics: &syn::Generics,
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
	let mut visit_bounds = compound_fields.visit.bounds;
	let visit_vocabulary_bounds = compound_fields.visit.vocabulary_bounds;
	let visit_body = &compound_fields.visit.body;

	let term = match compound_fields.id_field {
		Some((field_access, ty)) => {
			lexical_repr_bounds.push(syn::parse2(quote! {
				#ty: ::linked_data::Interpret<V, I>
			}).unwrap());

			visit_bounds.push(syn::parse2(quote! {
				#ty: ::linked_data::Interpret<V, I>
			}).unwrap());

			quote! {
				<#ty as ::linked_data::Interpret::<V, I>>::interpret(&#field_access, vocabulary, interpretation)
			}
		}
		None => quote! {
			::linked_data::ResourceInterpretation::Uninterpreted(None)
		},
	};

	let subject_id = format_ident!("_{ident}_{}", variant.ident);
	let input = &variant.input;

	let mut nest_generics = syn::Generics {
		lt_token: Some(Default::default()),
		params: Punctuated::new(),
		gt_token: Some(Default::default()),
		where_clause: None
	};

	nest_generics.params.push(syn::GenericParam::Lifetime(syn::LifetimeParam {
		attrs: Vec::new(),
		lifetime: syn::Lifetime::new("'_nest", Span::call_site()),
		colon_token: None,
		bounds: Punctuated::new()
	}));

	for p in &generics.params {
		if variant.uses_generic_param(p) {
			nest_generics.params.push(p.clone())
		}
	}

	let repr_generics = extend_generics(&nest_generics, VocabularyBounds::default(), lexical_repr_bounds.clone());
	let visit_generics = extend_generics(&nest_generics, visit_vocabulary_bounds, visit_bounds.clone());

	let (def_ty_generics, ty_generics, _) = nest_generics.split_for_impl();
	let (repr_impl_generics, _, repr_where_clauses) = repr_generics.split_for_impl();
	let (visit_impl_generics, _, visit_where_clauses) = visit_generics.split_for_impl();

	let definition = quote! {
		#[allow(non_camel_case_types)]
		struct #subject_id #def_ty_generics #borrowed_fields;

		impl #repr_impl_generics ::linked_data::Interpret<V, I> for #subject_id #ty_generics #repr_where_clauses {
			fn interpret(
				&self,
				vocabulary: &mut V,
				interpretation: &mut I,
			) -> linked_data::ResourceInterpretation<V, I> {
				let #subject_id #input = self;
				#term
			}
		}

		impl #repr_impl_generics ::linked_data::InterpretRef<'_nest, V, I> for #subject_id #ty_generics #repr_where_clauses {
			fn interpret_ref(
				&self,
				vocabulary: &mut V,
				interpretation: &mut I,
			) -> linked_data::ResourceInterpretation<'_nest, V, I> {
				let #subject_id #input = self;
				#term
			}
		}

		impl #visit_impl_generics ::linked_data::LinkedDataSubject<V, I> for #subject_id #ty_generics #visit_where_clauses {
			fn visit_subject<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
			where
				S: ::linked_data::SubjectVisitor<V, I>
			{
				let #subject_id #input = self;
				#visit_body
			}
		}

		impl #visit_impl_generics ::linked_data::LinkedDataPredicateObjects<V, I> for #subject_id #ty_generics #visit_where_clauses {
			fn visit_objects<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
			where
				S: ::linked_data::PredicateObjectsVisitor<V, I>
			{
				visitor.object(self)?;
				visitor.end()
			}
		}

		impl #visit_impl_generics ::linked_data::LinkedDataGraph<V, I> for #subject_id #ty_generics #visit_where_clauses {
			fn visit_graph<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
			where
				S: ::linked_data::GraphVisitor<V, I>
			{
				visitor.subject(self)?;
				visitor.end()
			}
		}

		impl #visit_impl_generics ::linked_data::LinkedData<V, I> for #subject_id #ty_generics #visit_where_clauses {
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
		lexical_repr_bounds,
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
	generics: &syn::Generics,
	variant: &StrippedVariant,
) -> Result<VariantShape, Error> {
	match &variant.fields {
		syn::Fields::Named(_) => Ok(VariantShape::Compound(variant_subject_type(
			attrs, ident, generics, variant,
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
				attrs, ident, generics, variant,
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
