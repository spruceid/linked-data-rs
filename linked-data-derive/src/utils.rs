pub trait UsesGenericParam {
	fn uses_generic_param(&self, p: &syn::GenericParam) -> bool;
}

impl UsesGenericParam for syn::Type {
	fn uses_generic_param(&self, p: &syn::GenericParam) -> bool {
		match self {
			Self::Array(a) => a.elem.uses_generic_param(p),
			Self::BareFn(t) => {
				t.inputs.iter().any(|i| i.ty.uses_generic_param(p))
					|| t.output.uses_generic_param(p)
			}
			Self::Group(g) => g.elem.uses_generic_param(p),
			Self::ImplTrait(t) => t.bounds.iter().any(|b| b.uses_generic_param(p)),
			Self::Paren(t) => t.elem.uses_generic_param(p),
			Self::Path(t) => t.uses_generic_param(p),
			Self::Ptr(t) => t.elem.uses_generic_param(p),
			Self::Reference(t) => {
				if let Some(l) = &t.lifetime {
					if l.uses_generic_param(p) {
						return true;
					}
				}

				t.elem.uses_generic_param(p)
			}
			Self::Slice(t) => t.elem.uses_generic_param(p),
			Self::TraitObject(t) => t.bounds.iter().any(|b| b.uses_generic_param(p)),
			Self::Tuple(t) => t.elems.iter().any(|t| t.uses_generic_param(p)),
			_ => false,
		}
	}
}

impl UsesGenericParam for syn::TypePath {
	fn uses_generic_param(&self, p: &syn::GenericParam) -> bool {
		if let Some(qself) = &self.qself {
			if qself.ty.uses_generic_param(p) {
				return true;
			}
		}

		self.path.uses_generic_param(p)
	}
}

impl UsesGenericParam for syn::ReturnType {
	fn uses_generic_param(&self, p: &syn::GenericParam) -> bool {
		match self {
			Self::Default => false,
			Self::Type(_, ty) => ty.uses_generic_param(p),
		}
	}
}

impl UsesGenericParam for syn::TypeParamBound {
	fn uses_generic_param(&self, p: &syn::GenericParam) -> bool {
		match self {
			Self::Lifetime(l) => l.uses_generic_param(p),
			Self::Trait(t) => t.uses_generic_param(p),
			_ => false,
		}
	}
}

impl UsesGenericParam for syn::Lifetime {
	fn uses_generic_param(&self, p: &syn::GenericParam) -> bool {
		match p {
			syn::GenericParam::Lifetime(l) => self.ident == l.lifetime.ident,
			_ => false,
		}
	}
}

impl UsesGenericParam for syn::TraitBound {
	fn uses_generic_param(&self, p: &syn::GenericParam) -> bool {
		self.path.uses_generic_param(p)
	}
}

impl UsesGenericParam for syn::Path {
	fn uses_generic_param(&self, p: &syn::GenericParam) -> bool {
		if let syn::GenericParam::Type(t) = p {
			if self.segments.len() == 1 && self.segments.first().unwrap().ident == t.ident {
				return true;
			}
		}

		self.segments.iter().any(|s| s.uses_generic_param(p))
	}
}

impl UsesGenericParam for syn::PathSegment {
	fn uses_generic_param(&self, p: &syn::GenericParam) -> bool {
		match &self.arguments {
			syn::PathArguments::None => false,
			syn::PathArguments::AngleBracketed(a) => a.args.iter().any(|a| a.uses_generic_param(p)),
			syn::PathArguments::Parenthesized(a) => {
				a.inputs.iter().any(|i| i.uses_generic_param(p)) || a.output.uses_generic_param(p)
			}
		}
	}
}

impl UsesGenericParam for syn::GenericArgument {
	fn uses_generic_param(&self, p: &syn::GenericParam) -> bool {
		match self {
			Self::AssocType(a) => a.uses_generic_param(p),
			Self::Constraint(c) => c.uses_generic_param(p),
			Self::Lifetime(l) => l.uses_generic_param(p),
			Self::Type(t) => t.uses_generic_param(p),
			_ => false,
		}
	}
}

impl UsesGenericParam for syn::AssocType {
	fn uses_generic_param(&self, p: &syn::GenericParam) -> bool {
		if let Some(generics) = &self.generics {
			if generics.args.iter().any(|a| a.uses_generic_param(p)) {
				return true;
			}
		}

		self.ty.uses_generic_param(p)
	}
}

impl UsesGenericParam for syn::Constraint {
	fn uses_generic_param(&self, p: &syn::GenericParam) -> bool {
		if let Some(generics) = &self.generics {
			if generics.args.iter().any(|a| a.uses_generic_param(p)) {
				return true;
			}
		}

		self.bounds.iter().any(|b| b.uses_generic_param(p))
	}
}
