use std::collections::HashMap;

use iref::{InvalidIri, IriBuf};
use proc_macro2::{Ident, Span, TokenStream, TokenTree};
use quote::{quote, ToTokens};
use syn::{spanned::Spanned, DeriveInput};

mod r#enum;
mod r#struct;

#[derive(Debug, thiserror::Error)]
pub enum Error {
	#[error("union types are not supported")]
	UnionType(Span),

	#[error("invalid `ld` attribute: {0}")]
	InvalidAttribute(AttributeError, Span),

	#[error("missing field serialization method")]
	UnknownFieldSerializationMethod(Span),

	#[error("invalid IRI `{0}`")]
	InvalidIri(String, Span),

	#[error("missing variant IRI")]
	MissingVariantIri(Span),
}

impl Error {
	pub fn span(&self) -> Span {
		match self {
			Self::UnionType(span) => *span,
			Self::InvalidAttribute(_, span) => *span,
			Self::UnknownFieldSerializationMethod(span) => *span,
			Self::InvalidIri(_, span) => *span,
			Self::MissingVariantIri(span) => *span,
		}
	}
}
#[derive(Debug, thiserror::Error)]
pub enum AttributeError {
	#[error("invalid shape")]
	InvalidShape,

	#[error("expected string literal")]
	ExpectedString,

	#[error("unknown attribute name")]
	UnknownIdent,

	#[error("empty")]
	Empty,

	#[error("unexpected token")]
	UnexpectedToken,

	#[error("invalid compact IRI")]
	InvalidCompactIri,

	#[error("missing `=`")]
	MissingEq,

	#[error("missing suffix string")]
	MissingSuffix,

	#[error("missing prefix binding")]
	MissingPrefixBinding,
}

pub struct CompactIri(IriBuf, Span);

impl CompactIri {
	pub fn expand(&self, prefixes: &HashMap<String, String>) -> Result<IriBuf, Error> {
		let (prefix, suffix) = self.0.split_once(':').unwrap();
		match prefixes.get(prefix) {
			Some(expanded_prefix) => IriBuf::new(format!("{expanded_prefix}{suffix}"))
				.map_err(|InvalidIri(s)| Error::InvalidIri(s, self.1)),
			None => Ok(self.0.clone()),
		}
	}
}

pub struct TypeAttributes {
	prefixes: HashMap<String, String>,
}

pub struct FieldAttributes {
	iri: Option<CompactIri>,
	flatten: bool,
	is_id: bool,
}

pub struct VariantAttributes {
	iri: Option<CompactIri>,
}

fn read_type_attributes(attributes: Vec<syn::Attribute>) -> Result<TypeAttributes, Error> {
	let mut result = TypeAttributes {
		prefixes: HashMap::new(),
	};

	for attr in attributes {
		if attr.path().is_ident("ld") {
			let span = attr.span();
			match attr.meta {
				syn::Meta::List(list) => {
					let mut tokens = list.tokens.into_iter();

					match tokens.next() {
						Some(TokenTree::Ident(id)) => {
							if id == "prefix" {
								match tokens.next() {
									Some(TokenTree::Group(g)) => {
										let (prefix, suffix) =
											parse_prefix_binding(g.stream(), span)?;
										result.prefixes.insert(prefix, suffix);
									}
									Some(token) => {
										return Err(Error::InvalidAttribute(
											AttributeError::UnexpectedToken,
											token.span(),
										))
									}
									None => {
										return Err(Error::InvalidAttribute(
											AttributeError::MissingPrefixBinding,
											span,
										))
									}
								}
							} else {
								return Err(Error::InvalidAttribute(
									AttributeError::UnknownIdent,
									id.span(),
								));
							}
						}
						Some(token) => {
							return Err(Error::InvalidAttribute(
								AttributeError::UnexpectedToken,
								token.span(),
							))
						}
						None => return Err(Error::InvalidAttribute(AttributeError::Empty, span)),
					}
				}
				_ => {
					return Err(Error::InvalidAttribute(
						AttributeError::InvalidShape,
						attr.span(),
					))
				}
			}
		}
	}

	Ok(result)
}

fn parse_prefix_binding(tokens: TokenStream, span: Span) -> Result<(String, String), Error> {
	let mut tokens = tokens.into_iter();
	match tokens.next() {
		Some(TokenTree::Literal(l)) => {
			let l = syn::Lit::new(l);
			match l {
				syn::Lit::Str(prefix) => match tokens.next() {
					Some(TokenTree::Punct(p)) if p.as_char() == '=' => match tokens.next() {
						Some(TokenTree::Literal(l)) => {
							let l = syn::Lit::new(l);
							match l {
								syn::Lit::Str(suffix) => Ok((prefix.value(), suffix.value())),
								l => Err(Error::InvalidAttribute(
									AttributeError::ExpectedString,
									l.span(),
								)),
							}
						}
						Some(token) => Err(Error::InvalidAttribute(
							AttributeError::UnexpectedToken,
							token.span(),
						)),
						None => Err(Error::InvalidAttribute(AttributeError::MissingSuffix, span)),
					},
					Some(token) => Err(Error::InvalidAttribute(
						AttributeError::UnexpectedToken,
						token.span(),
					)),
					None => Err(Error::InvalidAttribute(AttributeError::MissingEq, span)),
				},
				l => Err(Error::InvalidAttribute(
					AttributeError::ExpectedString,
					l.span(),
				)),
			}
		}
		Some(token) => Err(Error::InvalidAttribute(
			AttributeError::UnexpectedToken,
			token.span(),
		)),
		None => Err(Error::InvalidAttribute(AttributeError::Empty, span)),
	}
}

fn read_field_attributes(attributes: Vec<syn::Attribute>) -> Result<FieldAttributes, Error> {
	let mut iri = None;
	let mut flatten = false;
	let mut is_id = false;

	for attr in attributes {
		if attr.path().is_ident("ld") {
			let span = attr.span();
			match attr.meta {
				syn::Meta::List(list) => {
					let mut tokens = list.tokens.into_iter();
					match tokens.next() {
						Some(TokenTree::Ident(id)) => {
							if id == "flatten" {
								flatten = true
							} else if id == "id" {
								is_id = true
							} else {
								return Err(Error::InvalidAttribute(
									AttributeError::UnknownIdent,
									id.span(),
								));
							}
						}
						Some(TokenTree::Literal(l)) => {
							let l = syn::Lit::new(l);
							match l {
								syn::Lit::Str(l) => match IriBuf::new(l.value()) {
									Ok(value) => {
										iri = Some(CompactIri(value, l.span()));
									}
									Err(_) => {
										return Err(Error::InvalidAttribute(
											AttributeError::InvalidCompactIri,
											l.span(),
										))
									}
								},
								l => {
									return Err(Error::InvalidAttribute(
										AttributeError::ExpectedString,
										l.span(),
									))
								}
							}
						}
						Some(token) => {
							return Err(Error::InvalidAttribute(
								AttributeError::UnexpectedToken,
								token.span(),
							))
						}
						None => return Err(Error::InvalidAttribute(AttributeError::Empty, span)),
					}
				}
				_ => {
					return Err(Error::InvalidAttribute(
						AttributeError::InvalidShape,
						attr.span(),
					))
				}
			}
		}
	}

	Ok(FieldAttributes {
		iri,
		flatten,
		is_id,
	})
}

fn read_variant_attributes(attributes: Vec<syn::Attribute>) -> Result<VariantAttributes, Error> {
	let mut iri = None;

	for attr in attributes {
		if attr.path().is_ident("ld") {
			let span = attr.span();
			match attr.meta {
				syn::Meta::List(list) => {
					let mut tokens = list.tokens.into_iter();
					match tokens.next() {
						Some(TokenTree::Literal(l)) => {
							let l = syn::Lit::new(l);
							match l {
								syn::Lit::Str(l) => match IriBuf::new(l.value()) {
									Ok(value) => {
										iri = Some(CompactIri(value, l.span()));
									}
									Err(_) => {
										return Err(Error::InvalidAttribute(
											AttributeError::InvalidCompactIri,
											l.span(),
										))
									}
								},
								l => {
									return Err(Error::InvalidAttribute(
										AttributeError::ExpectedString,
										l.span(),
									))
								}
							}
						}
						Some(token) => {
							return Err(Error::InvalidAttribute(
								AttributeError::UnexpectedToken,
								token.span(),
							))
						}
						None => return Err(Error::InvalidAttribute(AttributeError::Empty, span)),
					}
				}
				_ => {
					return Err(Error::InvalidAttribute(
						AttributeError::InvalidShape,
						attr.span(),
					))
				}
			}
		}
	}

	Ok(VariantAttributes { iri })
}

pub fn subject(input: DeriveInput) -> Result<TokenStream, Error> {
	let attrs = read_type_attributes(input.attrs)?;
	match input.data {
		syn::Data::Struct(s) => r#struct::generate(&attrs, input.ident, s),
		syn::Data::Enum(e) => r#enum::generate(&attrs, input.ident, e),
		syn::Data::Union(u) => Err(Error::UnionType(u.union_token.span())),
	}
}

#[derive(Default, Clone, Copy)]
pub struct VocabularyBounds {
	iri_mut: bool,
}

impl VocabularyBounds {
	pub fn add(&mut self, other: Self) {
		self.iri_mut |= other.iri_mut
	}
}

impl ToTokens for VocabularyBounds {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		tokens.extend(quote! {
			::linked_data::rdf_types::Vocabulary
		});

		if self.iri_mut {
			tokens.extend(quote! {
				+ ::linked_data::rdf_types::IriVocabularyMut
			})
		}
	}
}

#[derive(Default)]
pub struct FieldsVisitor {
	bounds: Vec<TokenStream>,
	vocabulary_bounds: VocabularyBounds,
	body: TokenStream,
}

#[derive(Default)]
pub struct CompoundFields {
	visit: FieldsVisitor,
	id_field: Option<(TokenStream, syn::Type)>,
}

fn variant_compound_fields(
	attrs: &TypeAttributes,
	fields: syn::Fields,
	named_accessor: impl Fn(Ident) -> TokenStream,
	unnamed_accessor: impl Fn(u32) -> TokenStream,
	by_ref: impl Fn(TokenStream) -> TokenStream,
) -> Result<CompoundFields, Error> {
	let mut id_field = None;
	let mut visit = FieldsVisitor::default();

	let mut visit_fields = Vec::new();

	for (i, field) in fields.into_iter().enumerate() {
		let span = field.span();
		let field_attrs = read_field_attributes(field.attrs)?;

		let field_access = match field.ident {
			Some(id) => named_accessor(id),
			None => unnamed_accessor(i as u32),
		};

		let ty = field.ty;

		if field_attrs.is_id {
			id_field = Some((field_access, ty));
			continue;
		}

		let field_ref = by_ref(field_access);
		let visit_field = if field_attrs.flatten {
			visit.bounds.push(quote!(
				#ty: ::linked_data::LinkedDataSubject<V, I>
			));

			quote! {
				<#ty as ::linked_data::LinkedDataSubject<V, I>>::visit_subject(#field_ref, &mut visitor)?;
			}
		} else {
			match field_attrs.iri {
				Some(compact_iri) => {
					let iri = compact_iri.expand(&attrs.prefixes)?.into_string();
					visit.vocabulary_bounds.iri_mut = true;
					visit.bounds.push(quote!(
						#ty: ::linked_data::LinkedDataPredicateObjects<V, I>
					));

					quote! {
						visitor.predicate(
							::linked_data::iref::Iri::new(#iri).unwrap(),
							#field_ref
						)?;
					}
				}
				None => return Err(Error::UnknownFieldSerializationMethod(span)),
			}
		};

		visit_fields.push(visit_field)
	}

	visit.body = quote! {
		#(#visit_fields)*
		visitor.end()
	};

	Ok(CompoundFields { id_field, visit })
}
