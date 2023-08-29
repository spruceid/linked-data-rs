use proc_macro2::{TokenStream, Ident};
use quote::{ToTokens, quote};
use syn::{DeriveInput, spanned::Spanned};

use super::{Error, read_type_attributes, TypeAttributes, read_field_attributes};

mod r#enum;
mod r#struct;

pub fn subject(input: DeriveInput) -> Result<TokenStream, Error> {
	let attrs = read_type_attributes(input.attrs)?;
	match input.data {
		syn::Data::Struct(s) => r#struct::generate(&attrs, input.ident, s),
		syn::Data::Enum(e) => r#enum::generate(&attrs, input.ident, e),
		syn::Data::Union(u) => Err(Error::UnionType(u.union_token.span())),
	}
}