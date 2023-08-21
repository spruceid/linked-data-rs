use std::collections::HashMap;

use iref::{InvalidIri, IriBuf};
use proc_macro2::{Span, TokenStream, TokenTree};
use syn::spanned::Spanned;

pub mod ser;

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
    MissingVariantIri(Span)
}

impl Error {
    pub fn span(&self) -> Span {
        match self {
            Self::UnionType(span) => *span,
            Self::InvalidAttribute(_, span) => *span,
            Self::UnknownFieldSerializationMethod(span) => *span,
            Self::InvalidIri(_, span) => *span,
            Self::MissingVariantIri(span) => *span
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
    iri: Option<CompactIri>
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

    Ok(VariantAttributes {
        iri
    })
}