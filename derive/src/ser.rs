use proc_macro2::TokenStream;
use quote::quote;
use rdf_syntax::XSD_STRING;

use crate::{
	attributes::{EnumAttributes, FieldMode, StructRepresentation, parse_ld_attributes},
	compact_iri::IriPrefixes,
	error::Error,
	variant::LdVariant,
};

impl LdVariant {
	fn subjects_serializer(&self, prefixes: &IriPrefixes) -> Result<TokenStream, Error> {
		let transparent = self.is_transparent();
		let mut ser_subjects = Vec::with_capacity(self.fields.len());

		match &self.attrs.repr {
			Some(StructRepresentation::Id(id)) => {
				let id_iri = id.resolve(prefixes)?.into_string();
				ser_subjects.push(quote! {
					::linked_data::SerializeLinkedData::serialize_rdf(
						::linked_data::rdf_syntax::iri!(#id_iri),
						::linked_data::ser::FlattenSerializer::new(&mut serializer__, &mut subjects__),
						graph__
					)?;
				});
			}
			Some(StructRepresentation::Literal(value, ty)) => {
				let ty_iri = match ty {
					Some(ty) => ty.resolve(prefixes)?,
					None => XSD_STRING.to_owned(),
				}
				.into_string();

				ser_subjects.push(quote! {
					::linked_data::SerializeLinkedData::serialize_rdf(
						&::linked_data::rdf_syntax::LiteralRef::new(
							#value,
							::linked_data::rdf_syntax::iri!(#ty_iri)
						),
						::linked_data::ser::FlattenSerializer::new(&mut serializer__, &mut subjects__),
						graph__
					)?;
				});
			}
			None => (),
		}

		for f in &self.fields {
			if matches!(
				f.attrs.mode(f.ident.span(), transparent)?,
				Some(FieldMode::Flatten)
			) {
				let ident = &f.ident;

				ser_subjects.push(quote!(
					::linked_data::SerializeLinkedData::serialize_rdf(
						#ident,
						::linked_data::ser::FlattenSerializer::new(&mut serializer__, &mut subjects__),
						graph__
					)?;
				));
			}
		}

		Ok(quote! {
			let mut subjects__: Vec<R__> = Vec::new();

			#(#ser_subjects)*

			if subjects__.is_empty() {
				subjects__.push(::linked_data::LinkedDataSerializer::serialize_resource(
					&mut serializer__,
					None
				)?);
			}
		})
	}

	fn types_serializers(&self, prefixes: &IriPrefixes) -> Result<Vec<TokenStream>, Error> {
		let mut result = Vec::with_capacity(self.attrs.r#type.len());

		for ty in &self.attrs.r#type {
			let ty_iri = ty.resolve(prefixes)?.into_string();

			result.push(quote! {
				{
					let predicate__ = serializer__
						.interpret(Some(::linked_data::rdf_syntax::RDF_TYPE.into()))?;

					let object__ = serializer__
						.interpret(Some(::linked_data::rdf_syntax::iri!(#ty_iri).into()))?;

					serializer__.serialize_quad(::linked_data::rdf_syntax::Quad(
						subjects__[0].clone(),
						predicate__,
						object__,
						graph__.cloned()
					))?;
				}
			})
		}

		Ok(result)
	}

	fn props_serializer(&self, prefixes: &IriPrefixes) -> Result<Vec<TokenStream>, Error> {
		let transparent = self.is_transparent();
		let mut ser_props = Vec::with_capacity(self.fields.len());

		for f in &self.fields {
			if let Some(FieldMode::Prop(prop)) = f.attrs.mode(f.ident.span(), transparent)? {
				let prop_iri = prop.resolve(prefixes)?.into_string();
				let ident = &f.ident;

				ser_props.push(quote!(
					{
						let predicate__ = serializer__
							.interpret(Some(::linked_data::rdf_syntax::iri!(#prop_iri).into()))?;
						#ident.serialize_rdf_objects(
							::linked_data::LinkedDataSerializer::as_dyn_mut(&mut serializer__),
							&subjects__[0],
							&predicate__,
							graph__
						)?;
					}
				));
			}
		}

		Ok(ser_props)
	}

	fn serializer(&self, prefixes: &IriPrefixes) -> Result<TokenStream, Error> {
		let subjects = self.subjects_serializer(prefixes)?;
		let types = self.types_serializers(prefixes)?;
		let props = self.props_serializer(prefixes)?;

		Ok(quote! {
			#subjects
			#(#types)*
			#(#props)*

			serializer__.end()
		})
	}
}

/// Derives the `SerializeLinkedData` trait implementation.
pub fn derive(input: syn::DeriveInput) -> Result<TokenStream, Error> {
	let ident = input.ident;

	let body = match input.data {
		syn::Data::Struct(s) => {
			let mut prefixes = IriPrefixes::new(None);
			let v = LdVariant::new(&input.attrs, &s.fields)?;
			prefixes.try_extend(v.attrs.prefixes.iter().cloned())?;
			let destructor = v.pattern(&ident);
			let serializer = v.serializer(&prefixes)?;

			quote! {
				let #destructor = self;
				#serializer
			}
		}
		syn::Data::Enum(e) => {
			let mut prefixes = IriPrefixes::new(None);
			let attrs: EnumAttributes = parse_ld_attributes(&input.attrs)?;
			prefixes.try_extend(attrs.prefixes.iter().cloned())?;

			let mut serialize_variants = Vec::with_capacity(e.variants.len());

			for variant in &e.variants {
				let mut prefixes = IriPrefixes::new(Some(&prefixes));
				let v = LdVariant::new(&variant.attrs, &variant.fields)?;
				prefixes.try_extend(v.attrs.prefixes.iter().cloned())?;
				let destructor = v.pattern(&variant.ident);
				let serializer = v.serializer(&prefixes)?;

				serialize_variants.push(quote! {
					Self :: #destructor => {
						#serializer
					}
				});
			}

			quote! {
				match self {
					#(#serialize_variants),*
				}
			}
		}
		syn::Data::Union(u) => return Err(Error::Union(u.union_token.span)),
	};

	Ok(quote! {
		impl<R__: Clone + Ord> ::linked_data::SerializeLinkedData<R__> for #ident {
			fn serialize_rdf<S__>(&self, mut serializer__: S__, graph__: Option<&R__>) -> Result<S__::Ok, S__::Error>
			where
				S__: ::linked_data::LinkedDataSerializer<R__>
			{
				#body
			}
		}
	})
}
