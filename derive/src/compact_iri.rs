use std::collections::HashMap;

use proc_macro2::Span;
use rdf_syntax::IriBuf;

use crate::error::Error;

pub struct CompactIri(IriBuf, Span);

impl syn::parse::Parse for CompactIri {
	fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
		Self::new(input.parse()?)
	}
}

impl CompactIri {
	pub fn new(value: syn::LitStr) -> syn::Result<Self> {
		IriBuf::new(value.value())
			.map(|i| Self(i, value.span()))
			.map_err(|_| syn::parse::Error::new(value.span(), "Invalid compact IRI."))
	}

	pub fn span(&self) -> Span {
		self.1
	}

	pub fn resolve(&self, prefixes: &IriPrefixes) -> Result<IriBuf, Error> {
		let (prefix, suffix) = self.0.split_once(':').unwrap();

		match prefixes.get(prefix) {
			Some(prefix_value) => IriBuf::new(format!("{prefix_value}{suffix}"))
				.map_err(|_| Error::InvalidIri(self.1)),
			None => Ok(self.0.clone()),
		}
	}
}

#[derive(Default)]
pub struct IriPrefixes<'a> {
	parent: Option<&'a IriPrefixes<'static>>,
	map: HashMap<String, (IriBuf, Span)>,
}

impl<'a> IriPrefixes<'a> {
	pub fn new(parent: Option<&'a IriPrefixes<'static>>) -> Self {
		Self {
			parent,
			map: HashMap::new(),
		}
	}

	pub fn get(&self, prefix: &str) -> Option<&str> {
		self.map
			.get(prefix)
			.map(|(s, _)| s.as_str())
			.or_else(|| self.parent.and_then(|p| p.get(prefix)))
	}

	pub fn insert(&mut self, prefix: String, value: IriBuf, span: Span) -> Result<(), Error> {
		if self.get(&prefix).is_some() {
			return Err(Error::IriPrefixRedefinition(span));
		}

		self.map.insert(prefix, (value, span));

		Ok(())
	}

	pub fn try_extend(
		&mut self,
		items: impl IntoIterator<Item = (String, IriBuf, Span)>,
	) -> Result<(), Error> {
		for (p, v, s) in items {
			self.insert(p, v, s)?;
		}

		Ok(())
	}
}
