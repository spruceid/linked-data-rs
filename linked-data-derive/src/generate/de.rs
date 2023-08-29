use proc_macro2::TokenStream;

use syn::{spanned::Spanned, DeriveInput};

use super::{read_type_attributes, Error};

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
