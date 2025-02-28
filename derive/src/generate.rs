use std::collections::HashMap;

use iref::{InvalidIri, Iri, IriBuf};
use proc_macro2::{Span, TokenStream, TokenTree};
use quote::{format_ident, quote, ToTokens};
use static_iref::iri;

use syn::{punctuated::Punctuated, spanned::Spanned};

pub mod de;
pub mod ser;

const RDF_TYPE: &Iri = iri!("http://www.w3.org/1999/02/22-rdf-syntax-ns#type");

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

	#[error("missing type")]
	MissingType,

	#[error("invalid type")]
	InvalidType,
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
	type_: Option<CompactIri>,
}

pub struct FieldAttributes {
	ignore: bool,
	iri: Option<CompactIri>,
	flatten: bool,
	is_id: bool,
	graph_value: bool,
}

pub struct VariantAttributes {
	iri: Option<CompactIri>,
}

#[derive(Default, Clone, Copy)]
pub struct VocabularyBounds {
	pub iri_mut: bool,
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
				+ ::linked_data::rdf_types::vocabulary::IriVocabularyMut
			})
		}
	}
}

#[derive(Default, Clone, Copy)]
pub struct InterpretationBounds {
	pub iri_mut: bool,
	pub reverse_iri: bool,
}

impl InterpretationBounds {
	pub fn add(&mut self, other: Self) {
		self.iri_mut |= other.iri_mut;
		self.reverse_iri |= other.reverse_iri
	}
}

impl ToTokens for InterpretationBounds {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		tokens.extend(quote! {
			::linked_data::rdf_types::Interpretation
		});

		if self.iri_mut {
			tokens.extend(quote! {
				+ ::linked_data::rdf_types::interpretation::IriInterpretation<V_::Iri>
			})
		}

		if self.reverse_iri {
			tokens.extend(quote! {
				+ ::linked_data::rdf_types::interpretation::ReverseIriInterpretation<Iri = V_::Iri>
			})
		}
	}
}

fn extend_generics(
	generics: &syn::Generics,
	vocabulary_bounds: VocabularyBounds,
	interpertation_bounds: InterpretationBounds,
	mut bounds: Vec<syn::WherePredicate>,
) -> syn::Generics {
	let mut result = generics.clone();

	result.params.push(syn::GenericParam::Type(syn::TypeParam {
		attrs: Vec::new(),
		ident: format_ident!("I_"),
		colon_token: None,
		bounds: Punctuated::new(),
		eq_token: None,
		default: None,
	}));

	result.params.push(syn::GenericParam::Type(syn::TypeParam {
		attrs: Vec::new(),
		ident: format_ident!("V_"),
		colon_token: None,
		bounds: Punctuated::new(),
		eq_token: None,
		default: None,
	}));

	bounds.push(
		syn::parse2(quote! {
			I_: #interpertation_bounds
		})
		.unwrap(),
	);

	bounds.push(
		syn::parse2(quote! {
			V_: #vocabulary_bounds
		})
		.unwrap(),
	);

	let where_clause = result.where_clause.get_or_insert(syn::WhereClause {
		where_token: Default::default(),
		predicates: Punctuated::new(),
	});

	where_clause.predicates.extend(bounds);

	result
}

fn read_type_attributes(attributes: Vec<syn::Attribute>) -> Result<TypeAttributes, Error> {
	let mut result = TypeAttributes {
		prefixes: HashMap::new(),
		type_: None,
	};

	for attr in attributes {
		if attr.path().is_ident("ld") {
			let span = attr.span();
			match attr.meta {
				syn::Meta::List(list) => {
					let mut tokens = list.tokens.into_iter();

					while let Some(token) = tokens.next() {
						match token {
							TokenTree::Ident(id) => {
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
								} else if id == "type" {
									match tokens.next() {
										Some(TokenTree::Punct(p)) if p.as_char() == '=' => {
											match tokens.next() {
												Some(TokenTree::Literal(l)) => {
													let span = l.span();
													match syn::Lit::new(l) {
														syn::Lit::Str(s) => {
															match IriBuf::new(s.value()) {
																Ok(iri) => {
																	result.type_ =
																		Some(CompactIri(iri, span))
																}
																Err(_) => return Err(
																	Error::InvalidAttribute(
																		AttributeError::InvalidType,
																		span,
																	),
																),
															}
														}
														_ => {
															return Err(Error::InvalidAttribute(
																AttributeError::InvalidType,
																span,
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
												None => {
													return Err(Error::InvalidAttribute(
														AttributeError::MissingType,
														span,
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
										None => {
											return Err(Error::InvalidAttribute(
												AttributeError::MissingType,
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
							TokenTree::Punct(p) if p.as_char() == ',' => {}
							token => {
								return Err(Error::InvalidAttribute(
									AttributeError::UnexpectedToken,
									token.span(),
								))
							}
						}
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
	let mut ignore = false;
	let mut iri = None;
	let mut flatten = false;
	let mut is_id = false;
	let mut graph_value = false;

	for attr in attributes {
		if attr.path().is_ident("ld") {
			match attr.meta {
				syn::Meta::List(list) => {
					let mut tokens = list.tokens.into_iter();
					while let Some(token) = tokens.next() {
						match token {
							TokenTree::Ident(id) => {
								if id == "ignore" {
									ignore = true
								} else if id == "flatten" {
									flatten = true
								} else if id == "id" {
									is_id = true
								} else if id == "type" {
									iri = Some(CompactIri(RDF_TYPE.to_owned(), id.span()));
								} else if id == "graph" {
									graph_value = true
								} else {
									return Err(Error::InvalidAttribute(
										AttributeError::UnknownIdent,
										id.span(),
									));
								}
							}
							TokenTree::Literal(l) => {
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
							token => {
								return Err(Error::InvalidAttribute(
									AttributeError::UnexpectedToken,
									token.span(),
								))
							}
						}

						match tokens.next() {
							Some(TokenTree::Punct(p)) if p.as_char() == ',' => (),
							Some(token) => {
								return Err(Error::InvalidAttribute(
									AttributeError::UnexpectedToken,
									token.span(),
								))
							}
							None => break,
						}
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
		ignore,
		iri,
		flatten,
		is_id,
		graph_value,
	})
}

fn read_variant_attributes(attributes: Vec<syn::Attribute>) -> Result<VariantAttributes, Error> {
	let mut iri = None;

	for attr in attributes {
		if attr.path().is_ident("ld") {
			let span = attr.span();
			match attr.meta {
				syn::Meta::List(list) => {
					let VariantAttribute::Iri(i) = read_variant_attribute(list.tokens, span)?;
					iri = Some(i);
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

enum VariantAttribute {
	Iri(CompactIri),
}

fn read_variant_attribute(tokens: TokenStream, span: Span) -> Result<VariantAttribute, Error> {
	let mut tokens = tokens.into_iter();
	match tokens.next() {
		Some(TokenTree::Group(g)) => read_variant_attribute(g.stream(), span),
		Some(TokenTree::Literal(l)) => {
			let l = syn::Lit::new(l);
			match l {
				syn::Lit::Str(l) => match IriBuf::new(l.value()) {
					Ok(value) => Ok(VariantAttribute::Iri(CompactIri(value, l.span()))),
					Err(_) => Err(Error::InvalidAttribute(
						AttributeError::InvalidCompactIri,
						l.span(),
					)),
				},
				l => Err(Error::InvalidAttribute(
					AttributeError::ExpectedString,
					l.span(),
				)),
			}
		}
		Some(TokenTree::Ident(id)) if id == "type" => Ok(VariantAttribute::Iri(CompactIri(
			RDF_TYPE.to_owned(),
			id.span(),
		))),
		Some(token) => Err(Error::InvalidAttribute(
			AttributeError::UnexpectedToken,
			token.span(),
		)),
		None => Err(Error::InvalidAttribute(AttributeError::Empty, span)),
	}
}
