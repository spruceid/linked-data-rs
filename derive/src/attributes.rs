use proc_macro2::Span;
use rdf_syntax::IriBuf;
use syn::Token;

use crate::{compact_iri::CompactIri, error::Error};

pub fn parse_ld_attributes<T: LdAttributes>(attrs: &[syn::Attribute]) -> Result<T, Error> {
	let mut result = T::default();

	for attr in attrs {
		if attr.path().is_ident("ld") {
			result.join(attr.parse_args()?)?;
		}
	}

	Ok(result)
}

pub trait LdAttributes: syn::parse::Parse + Default {
	fn join(&mut self, other: Self) -> Result<(), Error>;
}

#[derive(Default)]
pub struct EnumAttributes {
	/// IRI prefixes.
	pub prefixes: Vec<(String, IriBuf, Span)>,
}

impl EnumAttributes {
	pub fn insert(&mut self, attr: EnumAttribute) -> Result<(), Error> {
		match attr {
			EnumAttribute::Prefix(p, v, s) => {
				self.prefixes.push((p, v, s));
			}
		}

		Ok(())
	}
}

impl LdAttributes for EnumAttributes {
	fn join(&mut self, other: Self) -> Result<(), Error> {
		for (p, v, s) in other.prefixes {
			self.insert(EnumAttribute::Prefix(p, v, s))?;
		}

		Ok(())
	}
}

impl syn::parse::Parse for EnumAttributes {
	fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
		let mut result = Self::default();

		for attr in input.parse_terminated(EnumAttribute::parse, syn::Token![,])? {
			result.insert(attr)?;
		}

		Ok(result)
	}
}

pub enum EnumAttribute {
	Prefix(String, IriBuf, Span),
}

impl syn::parse::Parse for EnumAttribute {
	fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
		let id: syn::Ident = input.parse()?;

		if id == "prefix" {
			let content;
			let _ = syn::parenthesized!(content in input);
			let prefix: syn::LitStr = content.parse()?;
			let _: syn::Token![=] = content.parse()?;
			let suffix: syn::LitStr = content.parse()?;

			Ok(Self::Prefix(
				prefix.value(),
				suffix
					.value()
					.try_into()
					.map_err(|_| Error::InvalidIri(suffix.span()))?,
				prefix.span(),
			))
		} else {
			Err(syn::parse::Error::new(id.span(), "Unknown enum attribute"))
		}
	}
}

pub enum StructRepresentation {
	Id(CompactIri),
	Literal(syn::LitStr, Option<CompactIri>),
}

impl StructRepresentation {
	pub fn span(&self) -> Span {
		match self {
			Self::Id(i) => i.span(),
			Self::Literal(l, _) => l.span(),
		}
	}
}

#[derive(Default)]
pub struct StructAttributes {
	/// IRI prefixes.
	pub prefixes: Vec<(String, IriBuf, Span)>,

	/// Lexical representation.
	pub repr: Option<StructRepresentation>,

	/// Variant type IRI.
	pub r#type: Vec<CompactIri>,

	/// Transparent representation.
	pub transparent: bool,
}

impl StructAttributes {
	pub fn insert(&mut self, attr: StructAttribute) -> Result<(), Error> {
		match attr {
			StructAttribute::Prefix(p, v, s) => {
				self.prefixes.push((p, v, s));
			}
			StructAttribute::Type(t) => self.r#type.push(t),
			StructAttribute::Id(i) => {
				if let Some(other) = self.repr.replace(StructRepresentation::Id(i)) {
					return Err(Error::AmbiguousRepr(other.span()));
				}
			}
			StructAttribute::Literal(l, t) => {
				if let Some(other) = self.repr.replace(StructRepresentation::Literal(l, t)) {
					return Err(Error::AmbiguousRepr(other.span()));
				}
			}
			StructAttribute::Transparent => self.transparent = true,
		}

		Ok(())
	}
}

impl LdAttributes for StructAttributes {
	fn join(&mut self, other: Self) -> Result<(), Error> {
		for (p, v, s) in other.prefixes {
			self.insert(StructAttribute::Prefix(p, v, s))?;
		}

		for t in other.r#type {
			self.insert(StructAttribute::Type(t))?;
		}

		match other.repr {
			Some(StructRepresentation::Id(i)) => self.insert(StructAttribute::Id(i))?,
			Some(StructRepresentation::Literal(l, ty)) => {
				self.insert(StructAttribute::Literal(l, ty))?
			}
			None => (),
		}

		if other.transparent {
			self.insert(StructAttribute::Transparent)?;
		}

		Ok(())
	}
}

impl syn::parse::Parse for StructAttributes {
	fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
		let mut result = Self::default();

		for attr in input.parse_terminated(StructAttribute::parse, syn::Token![,])? {
			result.insert(attr)?;
		}

		Ok(result)
	}
}

pub enum StructAttribute {
	Prefix(String, IriBuf, Span),
	Type(CompactIri),
	Id(CompactIri),
	Literal(syn::LitStr, Option<CompactIri>),
	Transparent,
}

impl syn::parse::Parse for StructAttribute {
	fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
		if input.peek(syn::Token![type]) {
			let _: syn::Token![type] = input.parse()?;
			let _: syn::Token![=] = input.parse()?;
			return Ok(Self::Type(input.parse()?));
		}

		let id: syn::Ident = input.parse()?;

		if id == "prefix" {
			let content;
			let _ = syn::parenthesized!(content in input);
			let prefix: syn::LitStr = content.parse()?;
			let _: syn::Token![=] = content.parse()?;
			let suffix: syn::LitStr = content.parse()?;

			Ok(Self::Prefix(
				prefix.value(),
				suffix
					.value()
					.try_into()
					.map_err(|_| Error::InvalidIri(suffix.span()))?,
				prefix.span(),
			))
		} else if id == "id" {
			let _: syn::Token![=] = input.parse()?;
			Ok(Self::Id(input.parse()?))
		} else if id == "literal" {
			let _: syn::Token![=] = input.parse()?;
			let value: syn::LitStr = input.parse()?;

			let ty = if input.peek(Token![:]) {
				let _: syn::Token![:] = input.parse()?;
				Some(input.parse()?)
			} else {
				None
			};

			Ok(Self::Literal(value, ty))
		} else if id == "transparent" {
			Ok(Self::Transparent)
		} else {
			Err(syn::parse::Error::new(
				id.span(),
				"Unknown struct/variant attribute",
			))
		}
	}
}

pub struct DefaultConstructor {
	pub span: Span,
	pub custom_function: Option<syn::Path>,
}

#[derive(Default)]
pub struct FieldAttributes {
	/// Property IRI.
	pub prop: Option<CompactIri>,

	/// Default constructor.
	pub default: Option<DefaultConstructor>,

	/// Should the field be flattened?
	pub flatten: bool,

	/// Should the field be ignored?
	///
	/// In this case, the default value is used on deserialization.
	pub skip: bool,
}

impl FieldAttributes {
	pub fn insert(&mut self, attr: FieldAttribute) -> Result<(), Error> {
		match attr {
			FieldAttribute::Prop(p) => {
				if let Some(other) = self.prop.replace(p) {
					return Err(Error::AmbiguousProp(other.span()));
				}
			}
			FieldAttribute::Default(d) => {
				if let Some(other) = self.default.replace(d) {
					return Err(Error::AmbiguousDefault(other.span));
				}
			}
			FieldAttribute::Flatten => self.flatten = true,
			FieldAttribute::Skip => self.skip = true,
		}

		Ok(())
	}

	pub fn can_be_transparent(&self) -> bool {
		self.prop.is_none() && !self.skip
	}

	pub fn mode(&self, span: Span, transparent: bool) -> Result<Option<FieldMode<'_>>, Error> {
		if self.skip {
			Ok(None)
		} else if transparent || self.flatten {
			Ok(Some(FieldMode::Flatten))
		} else if let Some(prop) = &self.prop {
			Ok(Some(FieldMode::Prop(prop)))
		} else {
			Err(Error::MissingFieldAnnotation(span))
		}
	}
}

pub enum FieldMode<'a> {
	Prop(&'a CompactIri),
	Flatten,
}

impl LdAttributes for FieldAttributes {
	fn join(&mut self, other: Self) -> Result<(), Error> {
		if let Some(p) = other.prop {
			self.insert(FieldAttribute::Prop(p))?;
		}

		if let Some(d) = other.default {
			self.insert(FieldAttribute::Default(d))?;
		}

		if other.flatten {
			self.insert(FieldAttribute::Flatten)?;
		}

		if other.skip {
			self.insert(FieldAttribute::Skip)?;
		}

		Ok(())
	}
}

impl syn::parse::Parse for FieldAttributes {
	fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
		let mut result = Self::default();

		for attr in input.parse_terminated(FieldAttribute::parse, syn::Token![,])? {
			result.insert(attr)?;
		}

		Ok(result)
	}
}

pub enum FieldAttribute {
	Prop(CompactIri),
	Default(DefaultConstructor),
	Flatten,
	Skip,
}

impl syn::parse::Parse for FieldAttribute {
	fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
		let id: syn::Ident = input.parse()?;

		if id == "prop" {
			let _: syn::Token![=] = input.parse()?;
			Ok(Self::Prop(input.parse()?))
		} else if id == "default" {
			let custom_function = if input.peek(syn::Token![=]) {
				let _: syn::Token![=] = input.parse()?;
				Some(input.parse()?)
			} else {
				None
			};

			Ok(Self::Default(DefaultConstructor {
				span: id.span(),
				custom_function,
			}))
		} else if id == "flatten" {
			Ok(Self::Flatten)
		} else if id == "skip" {
			Ok(Self::Skip)
		} else {
			Err(syn::parse::Error::new(id.span(), "Unknown field attribute"))
		}
	}
}
