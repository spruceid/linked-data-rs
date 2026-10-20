use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::spanned::Spanned;

use crate::{
	attributes::{FieldAttributes, StructAttributes, parse_ld_attributes},
	error::Error,
};

pub struct LdField {
	pub attrs: FieldAttributes,
	pub ident: syn::Ident,
	pub r#type: syn::Type,
}

pub enum LdVariantType {
	Unit,
	Named,
	Unnamed,
}

pub struct LdVariant {
	pub attrs: StructAttributes,
	pub r#type: LdVariantType,
	pub fields: Vec<LdField>,
}

impl LdVariant {
	pub fn new(attrs: &[syn::Attribute], fields: &syn::Fields) -> Result<Self, Error> {
		let attrs = parse_ld_attributes(attrs)?;

		let mut ld_fields = Vec::with_capacity(fields.len());

		let r#type = match fields {
			syn::Fields::Unit => LdVariantType::Unit,
			syn::Fields::Named(fields) => {
				for f in &fields.named {
					ld_fields.push(LdField {
						attrs: parse_ld_attributes(&f.attrs)?,
						ident: f.ident.clone().unwrap(),
						r#type: f.ty.clone(),
					});
				}

				LdVariantType::Named
			}
			syn::Fields::Unnamed(fields) => {
				for (i, f) in fields.unnamed.iter().enumerate() {
					ld_fields.push(LdField {
						attrs: parse_ld_attributes(&f.attrs)?,
						ident: format_ident!("__f_{i}", span = f.ty.span()),
						r#type: f.ty.clone(),
					});
				}

				LdVariantType::Unnamed
			}
		};

		Ok(Self {
			attrs,
			r#type,
			fields: ld_fields,
		})
	}

	pub fn is_unnamed(&self) -> bool {
		matches!(self.r#type, LdVariantType::Unnamed)
	}

	/// Checkes if this struct is transparent.
	///
	/// It is transparent if it is annotated with `#[ld(transparent)]` or has
	/// a single unnamed unannotated fields.
	pub fn is_transparent(&self) -> bool {
		self.attrs.transparent || (self.is_unnamed() && self.can_be_transparent())
	}

	/// Checks if this struct can be transparent.
	pub fn can_be_transparent(&self) -> bool {
		let mut non_transparent_field = false;

		for f in &self.fields {
			if !f.attrs.can_be_transparent() {
				if non_transparent_field {
					return false;
				}

				non_transparent_field = true;
			}
		}

		true
	}

	// /// Returns the subject field.
	// ///
	// /// This is either the one annotated with `#[ld(id)]`, or the transparent
	// /// field.
	// pub fn subject_field(&self) -> Result<Option<&LdField>, Error> {
	// 	let transparent = self.is_transparent();

	// 	let mut result = None;

	// 	for f in &self.fields {
	// 		if f.attrs.is_id || (transparent && !f.attrs.skip) {
	// 			if let Some(other) = result.replace(f) {
	// 				return Err(Error::AmbiguousId(other.ident.span()));
	// 			}
	// 		}
	// 	}

	// 	Ok(result)
	// }

	pub fn pattern(&self, ident: &syn::Ident) -> TokenStream {
		match self.r#type {
			LdVariantType::Unit => quote! {
				#ident
			},
			LdVariantType::Named => {
				let idents = self.fields.iter().map(|f| &f.ident);

				quote! {
					#ident {
						#(#idents,)*
					}
				}
			}
			LdVariantType::Unnamed => {
				let idents = self.fields.iter().map(|f| &f.ident);

				quote! {
					#ident (
						#(#idents,)*
					)
				}
			}
		}
	}
}
