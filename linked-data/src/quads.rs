use rdf_types::{
	Generator, Id, InsertIntoVocabulary, IriVocabularyMut, LiteralVocabularyMut, Quad, Term,
	Vocabulary,
};

use crate::{
	GraphVisitor, LexicalRepresentation, LinkedData, LinkedDataGraph, PredicateObjectsVisitor,
	RdfId, RdfQuad, SubjectVisitor, Visitor,
};

pub fn to_quads_with<V: Vocabulary, I>(
	vocabulary: &mut V,
	interpretation: &mut I,
	generator: impl Generator<V>,
	value: &impl LinkedData<V, I>,
) -> Result<Vec<RdfQuad<V>>, Error>
where
	V: IriVocabularyMut + LiteralVocabularyMut,
	V::BlankId: Clone,
	V::Iri: Clone,
	V::Literal: Clone,
	V::Value: From<String> + From<xsd_types::Value> + From<json_syntax::Value<()>>,
	V::Type: From<rdf_types::literal::Type<V::Iri, V::LanguageTag>>,
{
	value.visit(QuadSerializer {
		vocabulary,
		interpretation,
		generator,
		result: Vec::new(),
	})
}

pub fn to_quads(
	generator: impl Generator<()>,
	value: &impl LinkedData,
) -> Result<Vec<Quad>, Error> {
	value.visit(QuadSerializer {
		vocabulary: &mut (),
		interpretation: &mut (),
		generator,
		result: Vec::new(),
	})
}

fn generate_term<V: Vocabulary, I>(
	vocabulary: &mut V,
	interpretation: &mut I,
	generator: &mut impl Generator<V>,
	value: &(impl ?Sized + LexicalRepresentation<V, I>),
) -> Term<RdfId<V>, V::Literal>
where
	V: IriVocabularyMut + LiteralVocabularyMut,
	V::Value: From<String> + From<xsd_types::Value> + From<json_syntax::Value<()>>,
	V::Type: From<rdf_types::literal::Type<V::Iri, V::LanguageTag>>,
{
	match value.lexical_representation(interpretation, vocabulary) {
		Some(Term::Id(id)) => Term::Id(id),
		Some(Term::Literal(l)) => Term::Literal(l.insert_into_vocabulary(vocabulary)),
		None => Term::Id(generator.next(vocabulary)),
	}
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
	#[error("invalid graph label")]
	Graph,

	#[error("invalid subject")]
	Subject,

	#[error("invalid predicate")]
	Predicate,
}

/// A simple serializer generating a list of `Quad`s.
pub struct QuadSerializer<'a, V: Vocabulary, I, G> {
	vocabulary: &'a mut V,
	interpretation: &'a mut I,
	generator: G,
	result: Vec<RdfQuad<V>>,
}

impl<'a, V: Vocabulary, I, G> Visitor<V, I> for QuadSerializer<'a, V, I, G>
where
	V: IriVocabularyMut + LiteralVocabularyMut,
	V::BlankId: Clone,
	V::Iri: Clone,
	V::Literal: Clone,
	V::Value: From<String> + From<xsd_types::Value> + From<json_syntax::Value<()>>,
	V::Type: From<rdf_types::literal::Type<V::Iri, V::LanguageTag>>,
	G: Generator<V>,
{
	type Ok = Vec<RdfQuad<V>>;
	type Error = Error;

	fn default_graph<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + crate::LinkedDataGraph<V, I>,
	{
		let graph_serializer = QuadGraphSerializer {
			vocabulary: self.vocabulary,
			interpretation: self.interpretation,
			generator: &mut self.generator,
			result: &mut self.result,
			graph: None,
		};

		value.visit_graph(graph_serializer)
	}

	fn named_graph<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LexicalRepresentation<V, I> + crate::LinkedDataGraph<V, I>,
	{
		let graph = value
			.lexical_representation(self.interpretation, self.vocabulary)
			.unwrap_or_else(|| Term::Id(self.generator.next(self.vocabulary)))
			.into_id()
			.ok_or(Error::Graph)?;
		let graph_serializer = QuadGraphSerializer {
			vocabulary: self.vocabulary,
			interpretation: self.interpretation,
			generator: &mut self.generator,
			result: &mut self.result,
			graph: Some(&graph),
		};

		value.visit_graph(graph_serializer)
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(self.result)
	}
}

pub struct QuadGraphSerializer<'a, V: Vocabulary, I, G> {
	vocabulary: &'a mut V,
	interpretation: &'a mut I,
	generator: &'a mut G,
	result: &'a mut Vec<RdfQuad<V>>,
	graph: Option<&'a RdfId<V>>,
}

impl<'a, V: Vocabulary, I, G> GraphVisitor<V, I> for QuadGraphSerializer<'a, V, I, G>
where
	V: IriVocabularyMut + LiteralVocabularyMut,
	V::BlankId: Clone,
	V::Iri: Clone,
	V::Literal: Clone,
	V::Value: From<String> + From<xsd_types::Value> + From<json_syntax::Value<()>>,
	V::Type: From<rdf_types::literal::Type<V::Iri, V::LanguageTag>>,
	G: Generator<V>,
{
	type Ok = ();
	type Error = Error;

	fn subject<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LexicalRepresentation<V, I> + crate::LinkedDataSubject<V, I>,
	{
		let term = generate_term(self.vocabulary, self.interpretation, self.generator, value);
		let properties_serializer = QuadPropertiesSerializer {
			vocabulary: self.vocabulary,
			interpretation: self.interpretation,
			generator: self.generator,
			result: self.result,
			graph: self.graph,
			subject: &term,
		};

		value.visit_subject(properties_serializer)
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(())
	}
}

pub struct QuadPropertiesSerializer<'a, V: Vocabulary, I, G> {
	vocabulary: &'a mut V,
	interpretation: &'a mut I,
	generator: &'a mut G,
	result: &'a mut Vec<RdfQuad<V>>,
	graph: Option<&'a RdfId<V>>,
	subject: &'a Term<RdfId<V>, V::Literal>,
}

impl<'a, V: Vocabulary, I, G> SubjectVisitor<V, I> for QuadPropertiesSerializer<'a, V, I, G>
where
	V: IriVocabularyMut + LiteralVocabularyMut,
	V::BlankId: Clone,
	V::Iri: Clone,
	V::Literal: Clone,
	V::Value: From<String> + From<xsd_types::Value> + From<json_syntax::Value<()>>,
	V::Type: From<rdf_types::literal::Type<V::Iri, V::LanguageTag>>,
	G: Generator<V>,
{
	type Ok = ();
	type Error = Error;

	fn predicate<L, T>(&mut self, predicate: &L, value: &T) -> Result<(), Self::Error>
	where
		L: ?Sized + LexicalRepresentation<V, I>,
		T: ?Sized + crate::LinkedDataPredicateObjects<V, I>,
	{
		match self.subject {
			Term::Id(subject) => {
				let term = predicate
					.lexical_representation(self.interpretation, self.vocabulary)
					.unwrap_or_else(|| Term::Id(self.generator.next(self.vocabulary)));

				match term {
					Term::Id(Id::Iri(iri)) => {
						let objects_serializer = ObjectsSerializer {
							vocabulary: self.vocabulary,
							interpretation: self.interpretation,
							generator: self.generator,
							result: self.result,
							graph: self.graph,
							subject,
							predicate: iri,
						};

						value.visit_objects(objects_serializer)
					}
					_ => Err(Error::Predicate),
				}
			}
			_ => Err(Error::Subject),
		}
	}

	fn graph<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataGraph<V, I>,
	{
		let graph = match self.subject {
			Term::Id(id) => id,
			Term::Literal(_) => return Err(Error::Graph),
		};

		let graph_serializer = QuadGraphSerializer {
			vocabulary: self.vocabulary,
			interpretation: self.interpretation,
			generator: &mut self.generator,
			result: self.result,
			graph: Some(graph),
		};

		value.visit_graph(graph_serializer)
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(())
	}
}

pub struct ObjectsSerializer<'a, V: Vocabulary, I, G> {
	vocabulary: &'a mut V,
	interpretation: &'a mut I,
	generator: &'a mut G,
	result: &'a mut Vec<RdfQuad<V>>,
	graph: Option<&'a RdfId<V>>,
	subject: &'a RdfId<V>,
	predicate: V::Iri,
}

impl<'a, V: Vocabulary, I, G> PredicateObjectsVisitor<V, I> for ObjectsSerializer<'a, V, I, G>
where
	V: IriVocabularyMut + LiteralVocabularyMut,
	V::BlankId: Clone,
	V::Iri: Clone,
	V::Literal: Clone,
	V::Value: From<String> + From<xsd_types::Value> + From<json_syntax::Value<()>>,
	V::Type: From<rdf_types::literal::Type<V::Iri, V::LanguageTag>>,
	G: Generator<V>,
{
	type Ok = ();
	type Error = Error;

	fn object<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LexicalRepresentation<V, I> + crate::LinkedDataSubject<V, I>,
	{
		let term = generate_term(self.vocabulary, self.interpretation, self.generator, value);
		let subject_serializer = QuadPropertiesSerializer {
			vocabulary: self.vocabulary,
			interpretation: self.interpretation,
			generator: self.generator,
			result: self.result,
			graph: self.graph,
			subject: &term,
		};

		value.visit_subject(subject_serializer)?;
		self.result.push(Quad(
			self.subject.clone(),
			self.predicate.clone(),
			term,
			self.graph.cloned(),
		));
		Ok(())
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(())
	}
}
