use iref::{Iri, IriBuf};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use syn::spanned::Spanned;

use crate::generate::{read_variant_attributes, TypeAttributes, VariantAttributes};
use super::Error;

pub fn generate(
	attrs: &TypeAttributes,
	ident: Ident,
	e: syn::DataEnum,
) -> Result<TokenStream, Error> {
	Ok(quote!())
}