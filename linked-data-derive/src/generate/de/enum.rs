use proc_macro2::{Ident, TokenStream};
use quote::quote;

use super::Error;
use crate::generate::TypeAttributes;

pub fn generate(
	_attrs: &TypeAttributes,
	_ident: Ident,
	_e: syn::DataEnum,
) -> Result<TokenStream, Error> {
	Ok(quote!())
}
