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
	fn subjects_deserializer(&self, prefixes: &IriPrefixes) -> Result<TokenStream, Error> {
		let transparent = self.is_transparent();
		let mut de_subjects = Vec::with_capacity(self.fields.len());

		match &self.attrs.repr {
			Some(StructRepresentation::Id(id)) => {
				let id_iri = id.resolve(prefixes)?.into_string();
				de_subjects.push(quote! {
					{
						let subject__ = ::linked_data::LinkedDataDeserializer::interpret(
							  &mut deserializer__,
							  ::linked_data::rdf_syntax::iri!(#id_iri).into()
						   )?;

						subjects__.push(::linked_data::LinkedDataDeserializer::deserialize_resource(
							&mut deserializer__,
							Some(subject__)
						)?.ok_or_else(|| ::linked_data::de::Error::missing_resource())?);
					}
				});
			}
			Some(StructRepresentation::Literal(value, ty)) => {
				let ty_iri = match ty {
					Some(ty) => ty.resolve(prefixes)?,
					None => XSD_STRING.to_owned(),
				}
				.into_string();

				de_subjects.push(quote! {
					{
						let subject__ = ::linked_data::LinkedDataDeserializer::interpret(
							&mut deserializer__,
							::linked_data::rdf_syntax::LiteralRef::new(
								#value,
								::linked_data::rdf_syntax::iri!(#ty_iri)
							).into()
						)?;

						subjects__.push(::linked_data::LinkedDataDeserializer::deserialize_resource(
							&mut deserializer__,
							Some(subject__)
						)?.ok_or_else(|| ::linked_data::de::Error::missing_resource())?);
					}
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
				let ty = &f.r#type;

				let ident_expr = match &f.attrs.default {
					Some(d) => {
						let unwrap = match &d.custom_function {
							Some(path) => quote!(unwrap_or_else(#path)),
							None => quote!(unwrap_or_default()),
						};

						quote! {
							<Option<#ty> as ::linked_data::DeserializeLinkedData<R__>>::deserialize_rdf(
								::linked_data::de::FlattenDeserializer::new(&mut deserializer__, &mut subjects__),
								graph__
							)?.#unwrap
						}
					}
					None => {
						quote! {
							<#ty as ::linked_data::DeserializeLinkedData<R__>>::deserialize_rdf(
								::linked_data::de::FlattenDeserializer::new(&mut deserializer__, &mut subjects__),
								graph__
							)?
						}
					}
				};

				de_subjects.push(quote! {
					let #ident = #ident_expr;
				});
			}
		}

		Ok(quote! {
			let mut subjects__ = Vec::new();

			#(#de_subjects)*

			if subjects__.is_empty() {
				subjects__.push(
					::linked_data::LinkedDataDeserializer::deserialize_resource(&mut deserializer__, None)?
						.ok_or_else(|| ::linked_data::de::Error::missing_resource())?
				)
			}
		})
	}

	fn types_deserializer(&self, prefixes: &IriPrefixes) -> Result<TokenStream, Error> {
		if self.attrs.r#type.is_empty() {
			return Ok(quote!());
		}

		let mut de_types = Vec::with_capacity(self.fields.len());

		for ty in &self.attrs.r#type {
			let ty_iri = ty.resolve(prefixes)?.into_string();
			de_types.push(quote! {
				{
					let object__ = ::linked_data::LinkedDataDeserializer::interpret(
						  &mut deserializer__,
						  ::linked_data::rdf_syntax::iri!(#ty_iri).into()
					   )?;

					::linked_data::LinkedDataDeserializer::deserialize_quad(
						&mut deserializer__,
						::linked_data::rdf_syntax::Quad(
							Some(&subjects__[0]),
							Some(&predicate__),
							Some(&object__),
							Some(graph__)
						)
					)?.ok_or_else(::linked_data::de::Error::missing_quad)?;
				}
			});
		}

		Ok(quote! {
			{
				let predicate__ = ::linked_data::LinkedDataDeserializer::interpret(
					  &mut deserializer__,
					  ::linked_data::rdf_syntax::RDF_TYPE.into()
				   )?;

				#(#de_types)*
			}
		})
	}

	fn props_deserializer(&self, prefixes: &IriPrefixes) -> Result<Vec<TokenStream>, Error> {
		let transparent = self.is_transparent();
		let mut de_props = Vec::with_capacity(self.fields.len());

		for f in &self.fields {
			if let Some(FieldMode::Prop(prop)) = f.attrs.mode(f.ident.span(), transparent)? {
				let prop_iri = prop.resolve(prefixes)?.into_string();
				let ident = &f.ident;
				let ty = &f.r#type;

				let predicate = quote!(::linked_data::LinkedDataDeserializer::interpret(
					&mut deserializer__,
					::linked_data::rdf_syntax::iri!(#prop_iri).into()
				)?);

				let ident_def = match &f.attrs.default {
					Some(d) => {
						let unwrap = match &d.custom_function {
							Some(path) => quote!(unwrap_or_else(#path)),
							None => quote!(unwrap_or_default()),
						};

						quote! {
							<Option<#ty> as ::linked_data::DeserializeLinkedData<R__>>::deserialize_rdf_objects(
							::linked_data::LinkedDataDeserializer::as_dyn_mut(&mut deserializer__),
								&subjects__[0],
								&predicate__,
								graph__,
							)?.#unwrap
						}
					}
					None => {
						quote! {
							<#ty as ::linked_data::DeserializeLinkedData<R__>>::deserialize_rdf_objects(
								::linked_data::LinkedDataDeserializer::as_dyn_mut(&mut deserializer__),
								&subjects__[0],
								&predicate__,
								graph__,
							)?
						}
					}
				};

				de_props.push(quote! {
					let #ident = {
						let predicate__ = #predicate;
						#ident_def
					};
				})
			}
		}

		Ok(de_props)
	}

	/// Rejects `#[ld(graph)]` fields, which `DeserializeLinkedData` does not
	/// support yet: unlike serialization, there is no general way to
	/// discover a field's content back from a graph it isn't linked to by
	/// any subject/predicate.
	fn check_no_graph_fields(&self) -> Result<(), Error> {
		let transparent = self.is_transparent();

		for f in &self.fields {
			if matches!(
				f.attrs.mode(f.ident.span(), transparent)?,
				Some(FieldMode::Graph)
			) {
				return Err(Error::UnsupportedGraphFieldDeserialization(f.ident.span()));
			}
		}

		Ok(())
	}

	fn deserializer(&self, prefixes: &IriPrefixes) -> Result<TokenStream, Error> {
		self.check_no_graph_fields()?;

		let subjects = self.subjects_deserializer(prefixes)?;
		let types = self.types_deserializer(prefixes)?;
		let props = self.props_deserializer(prefixes)?;

		Ok(quote! {
			#subjects
			#types
			#(#props)*
		})
	}
}

/// Derives the `DeserializeLinkedData` trait implementation.
pub fn derive(input: syn::DeriveInput) -> Result<TokenStream, Error> {
	let ident = input.ident;

	let body = match input.data {
		syn::Data::Struct(s) => {
			let mut prefixes = IriPrefixes::new(None);
			let v = LdVariant::new(&input.attrs, &s.fields)?;
			prefixes.try_extend(v.attrs.prefixes.iter().cloned())?;
			let constructor = v.pattern(&ident);
			let deserializer = v.deserializer(&prefixes)?;

			quote! {
				#deserializer
				Ok(#constructor)
			}
		}
		syn::Data::Enum(e) => {
			let mut prefixes = IriPrefixes::new(None);
			let attrs: EnumAttributes = parse_ld_attributes(&input.attrs)?;
			prefixes.try_extend(attrs.prefixes.iter().cloned())?;

			let mut deserialize_variants = Vec::with_capacity(e.variants.len());

			for variant in e.variants {
				let mut prefixes = IriPrefixes::new(Some(&prefixes));
				let v = LdVariant::new(&variant.attrs, &variant.fields)?;
				prefixes.try_extend(v.attrs.prefixes.iter().cloned())?;
				let constructor = v.pattern(&variant.ident);
				let deserializer = v.deserializer(&prefixes)?;

				deserialize_variants.push(quote! {
					{
						let mut try__ = || {
							#deserializer
							Result::<Self, D__::Error>::Ok(Self:: #constructor)
						};

						match try__() {
							Ok(value) => {
								return Ok(value)
							},
							Err(e) => {
								deserializer__.reset()
							}
						}
					}
				});
			}

			quote! {
				let mut deserializer__ = ::linked_data::de::ReplayDeserializer::new(deserializer__);
				#(#deserialize_variants)*
				Err(<D__::Error as ::linked_data::de::Error<R__>>::custom("No matching enum variant."))
			}
		}
		syn::Data::Union(u) => return Err(Error::Union(u.union_token.span)),
	};

	Ok(quote! {
		impl<R__: Clone + Ord> ::linked_data::DeserializeLinkedData<R__> for #ident {
			fn deserialize_rdf<D__>(mut deserializer__: D__, graph__: Option<&R__>) -> Result<Self, D__::Error>
			where
				D__: ::linked_data::LinkedDataDeserializer<R__>
			{
				#body
			}
		}
	})
}
