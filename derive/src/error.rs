use proc_macro2::Span;

#[derive(Debug, thiserror::Error)]
pub enum Error {
	#[error(transparent)]
	Syntax(#[from] syn::parse::Error),

	#[error("Union type are not supported.")]
	Union(Span),

	#[error("Invalid IRI.")]
	InvalidIri(Span),

	#[error("IRI prefix already defined.")]
	IriPrefixRedefinition(Span),

	#[error("Missing field IRI.")]
	MissingFieldIri(Span),

	#[error("Ambiguous ID.")]
	AmbiguousRepr(Span),

	#[error("Ambiguous property.")]
	AmbiguousProp(Span),

	#[error("Ambiguous default.")]
	AmbiguousDefault(Span),

	#[error("Missing `ld` attribute on field.")]
	MissingFieldAnnotation(Span),
}

impl Error {
	pub fn span(&self) -> Span {
		match self {
			Self::Syntax(e) => e.span(),
			Self::Union(s) => *s,
			Self::InvalidIri(s) => *s,
			Self::IriPrefixRedefinition(s) => *s,
			Self::MissingFieldIri(s) => *s,
			Self::AmbiguousRepr(s) => *s,
			Self::AmbiguousProp(s) => *s,
			Self::AmbiguousDefault(s) => *s,
			Self::MissingFieldAnnotation(s) => *s,
		}
	}
}

impl From<Error> for syn::parse::Error {
	fn from(value: Error) -> Self {
		match value {
			Error::Syntax(e) => e,
			other => syn::parse::Error::new(other.span(), other),
		}
	}
}
