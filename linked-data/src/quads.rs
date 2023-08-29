use educe::Educe;
use rdf_types::{
	interpretation::{ReverseBlankIdInterpretation, ReverseIriInterpretation},
	BlankIdInterpretationMut, Generator, Id, InsertIntoVocabulary, Interpretation,
	InterpretationMut, IriInterpretationMut, IriVocabularyMut, LiteralInterpretationMut,
	LiteralVocabularyMut, Quad, ReverseLiteralInterpretation, Term, Vocabulary,
};

use crate::{
	CowRdfTerm, GraphVisitor, Interpret, InterpretedQuad, LinkedData, LinkedDataGraph,
	PredicateObjectsVisitor, RdfId, RdfQuad, ResourceInterpretation, SubjectVisitor, Visitor,
};

pub fn to_interpreted_quads<V: Vocabulary, I: Interpretation>(
	vocabulary: &mut V,
	interpretation: &mut I,
	value: &impl LinkedData<V, I>,
) -> Result<Vec<InterpretedQuad<I>>, Error>
where
	I: InterpretationMut
		+ IriInterpretationMut<V::Iri>
		+ BlankIdInterpretationMut<V::BlankId>
		+ LiteralInterpretationMut<V::Literal>,
	I::Resource: Clone,
	V: IriVocabularyMut + LiteralVocabularyMut,
	V::Iri: Clone,
	V::BlankId: Clone,
	V::Value: From<String> + From<xsd_types::Value> + From<json_syntax::Value<()>>,
	V::Type: From<rdf_types::literal::Type<V::Iri, V::LanguageTag>>,
	V::LanguageTag: Clone,
{
	value.visit(QuadSerializer {
		vocabulary,
		interpretation,
		domain: &mut InterpretationDomain,
		result: Vec::new(),
	})
}

pub fn to_quads_with<V: Vocabulary, I: Interpretation>(
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
	V::LanguageTag: Clone,
	I: ReverseIriInterpretation<Iri = V::Iri>
		+ ReverseBlankIdInterpretation<BlankId = V::BlankId>
		+ ReverseLiteralInterpretation<Literal = V::Literal>,
{
	let mut domain = VocabularyDomain { generator };

	value.visit(QuadSerializer {
		vocabulary,
		interpretation,
		domain: &mut domain,
		result: Vec::new(),
	})
}

pub fn to_quads(
	generator: impl Generator<()>,
	value: &impl LinkedData,
) -> Result<Vec<Quad>, Error> {
	let mut domain = VocabularyDomain { generator };

	value.visit(QuadSerializer {
		vocabulary: &mut (),
		interpretation: &mut (),
		domain: &mut domain,
		result: Vec::new(),
	})
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

trait Domain<V: Vocabulary, I: Interpretation> {
	type Subject: Clone;
	type Predicate: Clone;
	type Object;

	fn subject(
		&mut self,
		vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<V, I>,
	) -> Result<Self::Subject, Error>;

	fn predicate(
		&mut self,
		vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<V, I>,
	) -> Result<Self::Predicate, Error>;

	fn object(
		&mut self,
		vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<V, I>,
	) -> Result<Self::Object, Error>;

	fn graph(
		&mut self,
		vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<V, I>,
	) -> Result<Self::Subject, Error>;

	fn object_as_subject<'a>(&self, object: &'a Self::Object) -> Result<&'a Self::Subject, Error>;
}

type DomainQuad<V, I, D> = Quad<
	<D as Domain<V, I>>::Subject,
	<D as Domain<V, I>>::Predicate,
	<D as Domain<V, I>>::Object,
	<D as Domain<V, I>>::Subject,
>;

struct VocabularyDomain<G> {
	generator: G,
}

impl<V: Vocabulary, I: Interpretation, G: Generator<V>> Domain<V, I> for VocabularyDomain<G>
where
	V: IriVocabularyMut + LiteralVocabularyMut,
	V::Iri: Clone,
	V::BlankId: Clone,
	V::Literal: Clone,
	V::Value: From<String> + From<xsd_types::Value> + From<json_syntax::Value<()>>,
	V::Type: From<rdf_types::literal::Type<V::Iri, V::LanguageTag>>,
	V::LanguageTag: Clone,
	I: ReverseIriInterpretation<Iri = V::Iri>
		+ ReverseBlankIdInterpretation<BlankId = V::BlankId>
		+ ReverseLiteralInterpretation<Literal = V::Literal>,
{
	type Subject = RdfId<V>;
	type Predicate = V::Iri;
	type Object = Term<RdfId<V>, V::Literal>;

	fn subject(
		&mut self,
		vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<V, I>,
	) -> Result<Self::Subject, Error> {
		match value {
			ResourceInterpretation::Interpreted(r) => {
				if let Some(iri) = interpretation.iris_of(r).next() {
					return Ok(Id::Iri(iri.clone()));
				}

				if let Some(blank_id) = interpretation.blank_ids_of(r).next() {
					return Ok(Id::Blank(blank_id.clone()));
				}

				Ok(self.generator.next(vocabulary))
			}
			ResourceInterpretation::Uninterpreted(u) => u
				.map(|t| t.into_owned())
				.unwrap_or_else(|| Term::Id(self.generator.next(vocabulary)))
				.into_id()
				.ok_or(Error::Subject),
		}
	}

	fn predicate(
		&mut self,
		_vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<V, I>,
	) -> Result<Self::Predicate, Error> {
		match value {
			ResourceInterpretation::Interpreted(r) => {
				if let Some(iri) = interpretation.iris_of(r).next() {
					return Ok(iri.clone());
				}

				Err(Error::Predicate)
			}
			ResourceInterpretation::Uninterpreted(u) => match u {
				Some(CowRdfTerm::Owned(Term::Id(Id::Iri(iri)))) => Ok(iri),
				Some(CowRdfTerm::Borrowed(Term::Id(Id::Iri(iri)))) => Ok(iri.clone()),
				_ => Err(Error::Predicate),
			},
		}
	}

	fn object(
		&mut self,
		vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<V, I>,
	) -> Result<Self::Object, Error> {
		match value {
			ResourceInterpretation::Interpreted(r) => {
				if let Some(iri) = interpretation.iris_of(r).next() {
					return Ok(Term::Id(Id::Iri(iri.clone())));
				}

				if let Some(lit) = interpretation.literals_of(r).next() {
					return Ok(Term::Literal(lit.clone()));
				}

				if let Some(blank_id) = interpretation.blank_ids_of(r).next() {
					return Ok(Term::Id(Id::Blank(blank_id.clone())));
				}

				Ok(Term::Id(self.generator.next(vocabulary)))
			}
			ResourceInterpretation::Uninterpreted(u) => {
				let term = match u {
					Some(CowRdfTerm::Owned(Term::Id(id))) => Term::Id(id),
					Some(CowRdfTerm::Owned(Term::Literal(l))) => {
						Term::Literal(l.insert_into_vocabulary(vocabulary))
					}
					Some(CowRdfTerm::Borrowed(Term::Id(id))) => Term::Id(id.cloned()),
					Some(CowRdfTerm::Borrowed(Term::Literal(l))) => {
						Term::Literal(l.into_owned().insert_into_vocabulary(vocabulary))
					}
					None => Term::Id(self.generator.next(vocabulary)),
				};

				Ok(term)
			}
		}
	}

	fn graph(
		&mut self,
		vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<V, I>,
	) -> Result<Self::Subject, Error> {
		match value {
			ResourceInterpretation::Interpreted(r) => {
				if let Some(iri) = interpretation.iris_of(r).next() {
					return Ok(Id::Iri(iri.clone()));
				}

				if let Some(blank_id) = interpretation.blank_ids_of(r).next() {
					return Ok(Id::Blank(blank_id.clone()));
				}

				Ok(self.generator.next(vocabulary))
			}
			ResourceInterpretation::Uninterpreted(u) => u
				.map(CowRdfTerm::into_owned)
				.unwrap_or_else(|| Term::Id(self.generator.next(vocabulary)))
				.into_id()
				.ok_or(Error::Graph),
		}
	}

	fn object_as_subject<'a>(&self, object: &'a Self::Object) -> Result<&'a Self::Subject, Error> {
		match object {
			Term::Id(id) => Ok(id),
			Term::Literal(_) => Err(Error::Subject),
		}
	}
}

struct InterpretationDomain;

impl<V: Vocabulary, I: Interpretation> Domain<V, I> for InterpretationDomain
where
	I: InterpretationMut
		+ IriInterpretationMut<V::Iri>
		+ BlankIdInterpretationMut<V::BlankId>
		+ LiteralInterpretationMut<V::Literal>,
	I::Resource: Clone,
	V: IriVocabularyMut + LiteralVocabularyMut,
	V::Iri: Clone,
	V::BlankId: Clone,
	V::Value: From<String> + From<xsd_types::Value> + From<json_syntax::Value<()>>,
	V::Type: From<rdf_types::literal::Type<V::Iri, V::LanguageTag>>,
	V::LanguageTag: Clone,
{
	type Subject = I::Resource;
	type Predicate = I::Resource;
	type Object = I::Resource;

	fn subject(
		&mut self,
		_vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<V, I>,
	) -> Result<Self::Subject, Error> {
		match value {
			ResourceInterpretation::Interpreted(r) => Ok(r.clone()),
			ResourceInterpretation::Uninterpreted(u) => match u {
				Some(CowRdfTerm::Owned(Term::Id(Id::Iri(iri)))) => {
					Ok(interpretation.interpret_iri(iri))
				}
				Some(CowRdfTerm::Borrowed(Term::Id(Id::Iri(iri)))) => {
					Ok(interpretation.interpret_iri(iri.clone()))
				}
				Some(CowRdfTerm::Owned(Term::Id(Id::Blank(b)))) => {
					Ok(interpretation.interpret_blank_id(b))
				}
				Some(CowRdfTerm::Borrowed(Term::Id(Id::Blank(b)))) => {
					Ok(interpretation.interpret_blank_id(b.clone()))
				}
				Some(CowRdfTerm::Owned(Term::Literal(_))) => Err(Error::Subject),
				Some(CowRdfTerm::Borrowed(Term::Literal(_))) => Err(Error::Subject),
				None => Ok(interpretation.new_resource()),
			},
		}
	}

	fn predicate(
		&mut self,
		_vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<V, I>,
	) -> Result<Self::Predicate, Error> {
		match value {
			ResourceInterpretation::Interpreted(r) => Ok(r.clone()),
			ResourceInterpretation::Uninterpreted(u) => match u {
				Some(CowRdfTerm::Owned(Term::Id(Id::Iri(iri)))) => {
					Ok(interpretation.interpret_iri(iri))
				}
				Some(CowRdfTerm::Borrowed(Term::Id(Id::Iri(iri)))) => {
					Ok(interpretation.interpret_iri(iri.clone()))
				}
				_ => Err(Error::Predicate),
			},
		}
	}

	fn object(
		&mut self,
		vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<V, I>,
	) -> Result<Self::Object, Error> {
		match value {
			ResourceInterpretation::Interpreted(r) => Ok(r.clone()),
			ResourceInterpretation::Uninterpreted(u) => match u {
				Some(CowRdfTerm::Owned(Term::Id(Id::Iri(iri)))) => {
					Ok(interpretation.interpret_iri(iri))
				}
				Some(CowRdfTerm::Borrowed(Term::Id(Id::Iri(iri)))) => {
					Ok(interpretation.interpret_iri(iri.clone()))
				}
				Some(CowRdfTerm::Owned(Term::Id(Id::Blank(b)))) => {
					Ok(interpretation.interpret_blank_id(b))
				}
				Some(CowRdfTerm::Borrowed(Term::Id(Id::Blank(b)))) => {
					Ok(interpretation.interpret_blank_id(b.clone()))
				}
				Some(CowRdfTerm::Owned(Term::Literal(l))) => {
					let l = l.insert_into_vocabulary(vocabulary);
					let l = interpretation.interpret_literal(l);
					Ok(l)
				}
				Some(CowRdfTerm::Borrowed(Term::Literal(l))) => {
					let l = l.into_owned().insert_into_vocabulary(vocabulary);
					let l = interpretation.interpret_literal(l);
					Ok(l)
				}
				None => Ok(interpretation.new_resource()),
			},
		}
	}

	fn graph(
		&mut self,
		_vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<V, I>,
	) -> Result<Self::Subject, Error> {
		match value {
			ResourceInterpretation::Interpreted(r) => Ok(r.clone()),
			ResourceInterpretation::Uninterpreted(u) => match u {
				Some(CowRdfTerm::Owned(Term::Id(Id::Iri(iri)))) => {
					Ok(interpretation.interpret_iri(iri))
				}
				Some(CowRdfTerm::Borrowed(Term::Id(Id::Iri(iri)))) => {
					Ok(interpretation.interpret_iri(iri.clone()))
				}
				Some(CowRdfTerm::Owned(Term::Id(Id::Blank(b)))) => {
					Ok(interpretation.interpret_blank_id(b))
				}
				Some(CowRdfTerm::Borrowed(Term::Id(Id::Blank(b)))) => {
					Ok(interpretation.interpret_blank_id(b.clone()))
				}
				Some(CowRdfTerm::Owned(Term::Literal(_))) => Err(Error::Graph),
				Some(CowRdfTerm::Borrowed(Term::Literal(_))) => Err(Error::Graph),
				None => Ok(interpretation.new_resource()),
			},
		}
	}

	fn object_as_subject<'a>(&self, object: &'a Self::Object) -> Result<&'a Self::Subject, Error> {
		Ok(object)
	}
}

/// A simple serializer generating a list of `Quad`s.
struct QuadSerializer<'a, V: Vocabulary, I: Interpretation, D: Domain<V, I>> {
	vocabulary: &'a mut V,
	interpretation: &'a mut I,
	domain: &'a mut D,
	result: Vec<DomainQuad<V, I, D>>,
}

impl<'a, V: Vocabulary, I: Interpretation, D: Domain<V, I>> Visitor<V, I>
	for QuadSerializer<'a, V, I, D>
{
	type Ok = Vec<DomainQuad<V, I, D>>;
	type Error = Error;

	fn default_graph<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + crate::LinkedDataGraph<V, I>,
	{
		let graph_serializer = QuadGraphSerializer {
			vocabulary: self.vocabulary,
			interpretation: self.interpretation,
			domain: self.domain,
			result: &mut self.result,
			graph: None,
		};

		value.visit_graph(graph_serializer)
	}

	fn named_graph<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + Interpret<V, I> + crate::LinkedDataGraph<V, I>,
	{
		let i = value.interpret(self.vocabulary, self.interpretation);
		let graph = self.domain.graph(self.vocabulary, self.interpretation, i)?;

		let graph_serializer = QuadGraphSerializer {
			vocabulary: self.vocabulary,
			interpretation: self.interpretation,
			domain: self.domain,
			result: &mut self.result,
			graph: Some(&graph),
		};

		value.visit_graph(graph_serializer)
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(self.result)
	}
}

struct QuadGraphSerializer<'a, V: Vocabulary, I: Interpretation, D: Domain<V, I>> {
	vocabulary: &'a mut V,
	interpretation: &'a mut I,
	domain: &'a mut D,
	result: &'a mut Vec<DomainQuad<V, I, D>>,
	graph: Option<&'a D::Subject>,
}

impl<'a, V: Vocabulary, I: Interpretation, D: Domain<V, I>> GraphVisitor<V, I>
	for QuadGraphSerializer<'a, V, I, D>
{
	type Ok = ();
	type Error = Error;

	fn subject<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + Interpret<V, I> + crate::LinkedDataSubject<V, I>,
	{
		let i = value.interpret(self.vocabulary, self.interpretation);
		let term = self
			.domain
			.subject(self.vocabulary, self.interpretation, i)?;

		let properties_serializer = QuadPropertiesSerializer {
			vocabulary: self.vocabulary,
			interpretation: self.interpretation,
			domain: self.domain,
			result: self.result,
			graph: self.graph,
			subject: SubjectOrObject::Subject(&term),
		};

		value.visit_subject(properties_serializer)
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(())
	}
}

struct QuadPropertiesSerializer<'a, V: Vocabulary, I: Interpretation, D: Domain<V, I>> {
	vocabulary: &'a mut V,
	interpretation: &'a mut I,
	domain: &'a mut D,
	result: &'a mut Vec<DomainQuad<V, I, D>>,
	graph: Option<&'a D::Subject>,
	subject: SubjectOrObject<'a, V, I, D>,
}

#[derive(Educe)]
#[educe(Clone, Copy)]
enum SubjectOrObject<'a, V: Vocabulary, I: Interpretation, D: Domain<V, I>> {
	Subject(&'a D::Subject),
	Object(&'a D::Object),
}

impl<'a, V: Vocabulary, I: Interpretation, D: Domain<V, I>> SubjectOrObject<'a, V, I, D> {
	fn into_subject(self, domain: &D) -> Result<&'a D::Subject, Error> {
		match self {
			Self::Subject(s) => Ok(s),
			Self::Object(o) => domain.object_as_subject(o),
		}
	}
}

impl<'a, V: Vocabulary, I: Interpretation, D: Domain<V, I>> SubjectVisitor<V, I>
	for QuadPropertiesSerializer<'a, V, I, D>
{
	type Ok = ();
	type Error = Error;

	fn predicate<L, T>(&mut self, predicate: &L, value: &T) -> Result<(), Self::Error>
	where
		L: ?Sized + Interpret<V, I>,
		T: ?Sized + crate::LinkedDataPredicateObjects<V, I>,
	{
		let subject = self.subject.into_subject(self.domain)?;

		let i = predicate.interpret(self.vocabulary, self.interpretation);
		let term = self
			.domain
			.predicate(self.vocabulary, self.interpretation, i)?;

		let objects_serializer = ObjectsSerializer {
			vocabulary: self.vocabulary,
			interpretation: self.interpretation,
			domain: self.domain,
			result: self.result,
			graph: self.graph,
			subject,
			predicate: term,
		};

		value.visit_objects(objects_serializer)
	}

	fn graph<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataGraph<V, I>,
	{
		let graph = self.subject.into_subject(self.domain)?;

		let graph_serializer = QuadGraphSerializer {
			vocabulary: self.vocabulary,
			interpretation: self.interpretation,
			domain: self.domain,
			result: self.result,
			graph: Some(graph),
		};

		value.visit_graph(graph_serializer)
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(())
	}
}

struct ObjectsSerializer<'a, V: Vocabulary, I: Interpretation, D: Domain<V, I>> {
	vocabulary: &'a mut V,
	interpretation: &'a mut I,
	domain: &'a mut D,
	result: &'a mut Vec<DomainQuad<V, I, D>>,
	graph: Option<&'a D::Subject>,
	subject: &'a D::Subject,
	predicate: D::Predicate,
}

impl<'a, V: Vocabulary, I: Interpretation, D: Domain<V, I>> PredicateObjectsVisitor<V, I>
	for ObjectsSerializer<'a, V, I, D>
{
	type Ok = ();
	type Error = Error;

	fn object<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + Interpret<V, I> + crate::LinkedDataSubject<V, I>,
	{
		let i = value.interpret(self.vocabulary, self.interpretation);
		let term = self
			.domain
			.object(self.vocabulary, self.interpretation, i)?;
		let subject_serializer = QuadPropertiesSerializer {
			vocabulary: self.vocabulary,
			interpretation: self.interpretation,
			domain: self.domain,
			result: self.result,
			graph: self.graph,
			subject: SubjectOrObject::Object(&term),
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
