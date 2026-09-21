use educe::Educe;
use iref::IriBuf;
use rdf_types::{
	interpretation::{
		self, BlankIdInterpretationMut, IriInterpretationMut, LiteralInterpretationMut,
		ReverseBlankIdInterpretation, ReverseIriInterpretation, ReverseTermInterpretation,
		TermInterpretationMut,
	},
	vocabulary::{
		EmbedIntoVocabulary, ExtractedFromVocabulary, IriVocabularyMut, LiteralVocabularyMut,
	},
	Generator, Id, Interpretation, InterpretationMut, Quad, Term, Vocabulary,
};

use crate::{
	CowRdfTerm, GraphVisitor, InterpretedQuad, LinkedData, LinkedDataGraph, LinkedDataResource,
	LinkedDataSubject, PredicateObjectsVisitor, RdfId, RdfQuad, ResourceInterpretation,
	SubjectVisitor, Visitor,
};

pub fn to_interpreted_quads<I: Interpretation, V: Vocabulary>(
	vocabulary: &mut V,
	interpretation: &mut I,
	value: &impl LinkedData<I, V>,
) -> Result<Vec<InterpretedQuad<I>>, IntoQuadsError>
where
	I: InterpretationMut<V> + TermInterpretationMut<V::Iri, V::BlankId, V::Literal>,
	I::Resource: Clone,
	V: IriVocabularyMut + LiteralVocabularyMut,
	V::Iri: Clone,
	V::BlankId: Clone,
{
	value.visit(QuadSerializer {
		vocabulary,
		interpretation,
		domain: &mut InterpretationDomain,
		result: Vec::new(),
	})
}

pub fn to_interpreted_subject_quads<I: Interpretation, V: Vocabulary>(
	vocabulary: &mut V,
	interpretation: &mut I,
	graph: Option<&I::Resource>,
	value: &(impl LinkedDataSubject<I, V> + LinkedDataResource<I, V>),
) -> Result<(I::Resource, Vec<InterpretedQuad<I>>), IntoQuadsError>
where
	I: InterpretationMut<V>
		+ IriInterpretationMut<V::Iri>
		+ BlankIdInterpretationMut<V::BlankId>
		+ LiteralInterpretationMut<V::Literal>,
	I::Resource: Clone,
	V: IriVocabularyMut + LiteralVocabularyMut,
	V::Iri: Clone,
	V::BlankId: Clone,
{
	let mut result = Vec::new();

	let subject = match value.interpretation(vocabulary, interpretation) {
		ResourceInterpretation::Interpreted(r) => r.clone(),
		ResourceInterpretation::Uninterpreted(_) => interpretation.new_resource(vocabulary),
	};

	value.visit_subject(QuadPropertiesSerializer {
		vocabulary,
		interpretation,
		domain: &mut InterpretationDomain,
		graph,
		subject: SubjectOrObject::Subject(&subject),
		result: &mut result,
	})?;

	Ok((subject, result))
}

pub fn to_interpreted_graph_quads<I: Interpretation, V: Vocabulary>(
	vocabulary: &mut V,
	interpretation: &mut I,
	value: &(impl LinkedDataGraph<I, V> + LinkedDataResource<I, V>),
) -> Result<(I::Resource, Vec<InterpretedQuad<I>>), IntoQuadsError>
where
	I: InterpretationMut<V>
		+ IriInterpretationMut<V::Iri>
		+ BlankIdInterpretationMut<V::BlankId>
		+ LiteralInterpretationMut<V::Literal>,
	I::Resource: Clone,
	V: IriVocabularyMut + LiteralVocabularyMut,
	V::Iri: Clone,
	V::BlankId: Clone,
{
	let mut result = Vec::new();

	let graph = match value.interpretation(vocabulary, interpretation) {
		ResourceInterpretation::Interpreted(r) => r.clone(),
		ResourceInterpretation::Uninterpreted(_) => interpretation.new_resource(vocabulary),
	};

	value.visit_graph(QuadGraphSerializer {
		vocabulary,
		interpretation,
		domain: &mut InterpretationDomain,
		graph: Some(&graph),
		result: &mut result,
	})?;

	Ok((graph, result))
}

pub fn to_lexical_quads_with<I: Interpretation, V: Vocabulary>(
	vocabulary: &mut V,
	interpretation: &mut I,
	value: &impl LinkedData<I, V>,
) -> Result<Vec<RdfQuad>, IntoQuadsError>
where
	I: InterpretationMut<V>
		+ ReverseTermInterpretation<Iri = V::Iri, BlankId = V::BlankId, Literal = V::Literal>,
{
	let mut domain = LexicalDomain;

	value.visit(QuadSerializer {
		vocabulary,
		interpretation,
		domain: &mut domain,
		result: Vec::new(),
	})
}

pub fn to_lexical_subject_quads_with<I: Interpretation, V: Vocabulary>(
	vocabulary: &mut V,
	interpretation: &mut I,
	graph: Option<&Id>,
	value: &(impl LinkedDataSubject<I, V> + LinkedDataResource<I, V>),
) -> Result<(Id, Vec<RdfQuad>), IntoQuadsError>
where
	I: InterpretationMut<V>
		+ ReverseTermInterpretation<Iri = V::Iri, BlankId = V::BlankId, Literal = V::Literal>,
	I::Resource: Clone,
{
	let mut result = Vec::new();

	let i = value.interpretation(vocabulary, interpretation);
	let subject = LexicalDomain.subject(vocabulary, interpretation, i)?;

	value.visit_subject(QuadPropertiesSerializer {
		vocabulary,
		interpretation,
		domain: &mut LexicalDomain,
		graph,
		subject: SubjectOrObject::Subject(&subject),
		result: &mut result,
	})?;

	Ok((subject, result))
}

pub fn to_lexical_quads<G: Generator>(
	generator: G,
	value: &impl LinkedData<interpretation::WithGenerator<G>>,
) -> Result<Vec<RdfQuad>, IntoQuadsError> {
	let mut interpretation = rdf_types::interpretation::WithGenerator::new((), generator);
	to_lexical_quads_with(&mut (), &mut interpretation, value)
}

pub fn to_lexical_subject_quads<G: Generator>(
	generator: G,
	graph: Option<&Id>,
	value: &(impl LinkedDataSubject<interpretation::WithGenerator<G>>
	      + LinkedDataResource<interpretation::WithGenerator<G>>),
) -> Result<(Id, Vec<RdfQuad>), IntoQuadsError> {
	let mut interpretation = rdf_types::interpretation::WithGenerator::new((), generator);
	to_lexical_subject_quads_with(&mut (), &mut interpretation, graph, value)
}

pub fn to_quads_with<I: InterpretationMut<V>, V: Vocabulary>(
	vocabulary: &mut V,
	interpretation: &mut I,
	value: &impl LinkedData<I, V>,
) -> Result<Vec<RdfQuad<V>>, IntoQuadsError>
where
	V: IriVocabularyMut + LiteralVocabularyMut,
	V::BlankId: Clone,
	V::Iri: Clone,
	V::Literal: Clone,
	I: ReverseTermInterpretation<Iri = V::Iri, BlankId = V::BlankId, Literal = V::Literal>,
{
	let mut domain = VocabularyDomain;

	value.visit(QuadSerializer {
		vocabulary,
		interpretation,
		domain: &mut domain,
		result: Vec::new(),
	})
}

pub fn to_quads<G: Generator>(
	generator: G,
	value: &impl LinkedData<interpretation::WithGenerator<G>>,
) -> Result<Vec<RdfQuad>, IntoQuadsError> {
	let mut interpretation = interpretation::WithGenerator::new((), generator);
	to_quads_with(&mut (), &mut interpretation, value)
}

#[derive(Debug, thiserror::Error)]
pub enum IntoQuadsError {
	#[error("invalid graph label")]
	Graph,

	#[error("invalid subject")]
	Subject,

	#[error("invalid predicate")]
	Predicate,

	#[error("missing lexical representation")]
	MissingLexicalRepresentation,
}

trait Domain<I: Interpretation, V: Vocabulary> {
	type Subject: Clone;
	type Predicate: Clone;
	type Object;
	type ObjectRef<'a>: Copy
	where
		V::Iri: 'a,
		V::BlankId: 'a,
		V::Literal: 'a,
		I::Resource: 'a;

	fn subject(
		&mut self,
		vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<I, V>,
	) -> Result<Self::Subject, IntoQuadsError>;

	fn predicate(
		&mut self,
		vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<I, V>,
	) -> Result<Self::Predicate, IntoQuadsError>;

	fn object(
		&mut self,
		vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<I, V>,
	) -> Result<Self::Object, IntoQuadsError>;

	fn graph(
		&mut self,
		vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<I, V>,
	) -> Result<Self::Subject, IntoQuadsError>;

	fn object_as_subject<'a>(
		&self,
		object: &'a Self::Object,
	) -> Result<&'a Self::Subject, IntoQuadsError>;

	fn subject_as_object<'a>(
		&self,
		subject: &'a Self::Subject,
	) -> Result<Self::ObjectRef<'a>, IntoQuadsError>
	where
		V::Iri: 'a,
		V::BlankId: 'a,
		V::Literal: 'a,
		I::Resource: 'a;

	fn object_as_ref<'a>(object: &'a Self::Object) -> Self::ObjectRef<'a>
	where
		V::Iri: 'a,
		V::BlankId: 'a,
		V::Literal: 'a,
		I::Resource: 'a;

	fn cloned_object_ref<'a>(object_ref: Self::ObjectRef<'a>) -> Self::Object
	where
		V::Iri: 'a,
		V::BlankId: 'a,
		V::Literal: 'a,
		I::Resource: 'a;
}

type DomainQuad<I, V, D> = Quad<
	<D as Domain<I, V>>::Subject,
	<D as Domain<I, V>>::Predicate,
	<D as Domain<I, V>>::Object,
	<D as Domain<I, V>>::Subject,
>;

struct VocabularyDomain;

#[allow(clippy::type_complexity)]
fn resource_term<I>(
	interpretation: &I,
	r: &I::Resource,
) -> Result<Term<Id<I::Iri, I::BlankId>, I::Literal>, IntoQuadsError>
where
	I: ReverseTermInterpretation,
	I::Iri: Clone,
	I::BlankId: Clone,
	I::Literal: Clone,
{
	if let Some(iri) = interpretation.iris_of(r).next() {
		return Ok(Term::Id(Id::Iri(iri.clone())));
	}

	if let Some(lit) = interpretation.literals_of(r).next() {
		return Ok(Term::Literal(lit.clone()));
	}

	if let Some(blank_id) = interpretation.blank_ids_of(r).next() {
		return Ok(Term::Id(Id::Blank(blank_id.clone())));
	}

	Err(IntoQuadsError::MissingLexicalRepresentation)
}

impl<I: InterpretationMut<V>, V: Vocabulary> Domain<I, V> for VocabularyDomain
where
	V: IriVocabularyMut + LiteralVocabularyMut,
	V::Iri: Clone,
	V::BlankId: Clone,
	V::Literal: Clone,
	I: ReverseTermInterpretation<Iri = V::Iri, BlankId = V::BlankId, Literal = V::Literal>,
{
	type Subject = RdfId<V>;
	type Predicate = V::Iri;
	type Object = Term<RdfId<V>, V::Literal>;
	type ObjectRef<'a>
		= Term<&'a RdfId<V>, &'a V::Literal>
	where
		V::Iri: 'a,
		V::BlankId: 'a,
		V::Literal: 'a,
		I::Resource: 'a;

	fn subject(
		&mut self,
		vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<I, V>,
	) -> Result<Self::Subject, IntoQuadsError> {
		match value {
			ResourceInterpretation::Interpreted(r) => {
				if let Some(iri) = interpretation.iris_of(r).next() {
					return Ok(Id::Iri(iri.clone()));
				}

				if let Some(blank_id) = interpretation.blank_ids_of(r).next() {
					return Ok(Id::Blank(blank_id.clone()));
				}

				Err(IntoQuadsError::MissingLexicalRepresentation)
			}
			ResourceInterpretation::Uninterpreted(u) => match u {
				Some(u) => u.into_owned().into_id(),
				None => {
					let r = interpretation.new_resource(vocabulary);
					resource_term(interpretation, &r)?.into_id()
				}
			}
			.ok_or(IntoQuadsError::Subject),
		}
	}

	fn predicate(
		&mut self,
		_vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<I, V>,
	) -> Result<Self::Predicate, IntoQuadsError> {
		match value {
			ResourceInterpretation::Interpreted(r) => {
				if let Some(iri) = interpretation.iris_of(r).next() {
					return Ok(iri.clone());
				}

				Err(IntoQuadsError::Predicate)
			}
			ResourceInterpretation::Uninterpreted(u) => match u {
				Some(CowRdfTerm::Owned(Term::Id(Id::Iri(iri)))) => Ok(iri),
				Some(CowRdfTerm::Borrowed(Term::Id(Id::Iri(iri)))) => Ok(iri.clone()),
				_ => Err(IntoQuadsError::Predicate),
			},
		}
	}

	fn object(
		&mut self,
		vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<I, V>,
	) -> Result<Self::Object, IntoQuadsError> {
		match value {
			ResourceInterpretation::Interpreted(r) => resource_term(interpretation, r),
			ResourceInterpretation::Uninterpreted(u) => {
				let term = match u {
					Some(CowRdfTerm::Owned(Term::Id(id))) => Term::Id(id),
					Some(CowRdfTerm::Owned(Term::Literal(l))) => {
						Term::Literal(l.embed_into_vocabulary(vocabulary))
					}
					Some(CowRdfTerm::Borrowed(Term::Id(id))) => Term::Id(id.cloned()),
					Some(CowRdfTerm::Borrowed(Term::Literal(l))) => {
						Term::Literal(l.into_owned().embed_into_vocabulary(vocabulary))
					}
					None => {
						let r = interpretation.new_resource(vocabulary);
						resource_term(interpretation, &r)?
					}
				};

				Ok(term)
			}
		}
	}

	fn graph(
		&mut self,
		vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<I, V>,
	) -> Result<Self::Subject, IntoQuadsError> {
		match value {
			ResourceInterpretation::Interpreted(r) => {
				if let Some(iri) = interpretation.iris_of(r).next() {
					return Ok(Id::Iri(iri.clone()));
				}

				if let Some(blank_id) = interpretation.blank_ids_of(r).next() {
					return Ok(Id::Blank(blank_id.clone()));
				}

				Err(IntoQuadsError::MissingLexicalRepresentation)
			}
			ResourceInterpretation::Uninterpreted(u) => match u {
				Some(u) => u.into_owned().into_id(),
				None => {
					let r = interpretation.new_resource(vocabulary);
					resource_term(interpretation, &r)?.into_id()
				}
			}
			.ok_or(IntoQuadsError::Subject),
		}
	}

	fn object_as_subject<'a>(
		&self,
		object: &'a Self::Object,
	) -> Result<&'a Self::Subject, IntoQuadsError> {
		match object {
			Term::Id(id) => Ok(id),
			Term::Literal(_) => Err(IntoQuadsError::Subject),
		}
	}

	fn subject_as_object<'a>(
		&self,
		subject: &'a Self::Subject,
	) -> Result<Self::ObjectRef<'a>, IntoQuadsError>
	where
		V::Iri: 'a,
		V::BlankId: 'a,
		V::Literal: 'a,
		I::Resource: 'a,
	{
		Ok(Term::Id(subject))
	}

	fn object_as_ref<'a>(object: &'a Self::Object) -> Self::ObjectRef<'a>
	where
		V::Iri: 'a,
		V::BlankId: 'a,
		V::Literal: 'a,
		I::Resource: 'a,
	{
		object.as_ref()
	}

	fn cloned_object_ref<'a>(object_ref: Self::ObjectRef<'a>) -> Self::Object
	where
		V::Iri: 'a,
		V::BlankId: 'a,
		V::Literal: 'a,
		I::Resource: 'a,
	{
		object_ref.cloned()
	}
}

struct InterpretationDomain;

impl<I: Interpretation, V: Vocabulary> Domain<I, V> for InterpretationDomain
where
	I: InterpretationMut<V>
		+ IriInterpretationMut<V::Iri>
		+ BlankIdInterpretationMut<V::BlankId>
		+ LiteralInterpretationMut<V::Literal>,
	I::Resource: Clone,
	V: IriVocabularyMut + LiteralVocabularyMut,
	V::Iri: Clone,
	V::BlankId: Clone,
{
	type Subject = I::Resource;
	type Predicate = I::Resource;
	type Object = I::Resource;
	type ObjectRef<'a>
		= &'a I::Resource
	where
		V::Iri: 'a,
		V::BlankId: 'a,
		V::Literal: 'a,
		I::Resource: 'a;

	fn subject(
		&mut self,
		vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<I, V>,
	) -> Result<Self::Subject, IntoQuadsError> {
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
				Some(CowRdfTerm::Owned(Term::Literal(_))) => Err(IntoQuadsError::Subject),
				Some(CowRdfTerm::Borrowed(Term::Literal(_))) => Err(IntoQuadsError::Subject),
				None => Ok(interpretation.new_resource(vocabulary)),
			},
		}
	}

	fn predicate(
		&mut self,
		_vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<I, V>,
	) -> Result<Self::Predicate, IntoQuadsError> {
		match value {
			ResourceInterpretation::Interpreted(r) => Ok(r.clone()),
			ResourceInterpretation::Uninterpreted(u) => match u {
				Some(CowRdfTerm::Owned(Term::Id(Id::Iri(iri)))) => {
					Ok(interpretation.interpret_iri(iri))
				}
				Some(CowRdfTerm::Borrowed(Term::Id(Id::Iri(iri)))) => {
					Ok(interpretation.interpret_iri(iri.clone()))
				}
				_ => Err(IntoQuadsError::Predicate),
			},
		}
	}

	fn object(
		&mut self,
		vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<I, V>,
	) -> Result<Self::Object, IntoQuadsError> {
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
					let l = l.embed_into_vocabulary(vocabulary);
					let l = interpretation.interpret_literal(l);
					Ok(l)
				}
				Some(CowRdfTerm::Borrowed(Term::Literal(l))) => {
					let l = l.into_owned().embed_into_vocabulary(vocabulary);
					let l = interpretation.interpret_literal(l);
					Ok(l)
				}
				None => Ok(interpretation.new_resource(vocabulary)),
			},
		}
	}

	fn graph(
		&mut self,
		vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<I, V>,
	) -> Result<Self::Subject, IntoQuadsError> {
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
				Some(CowRdfTerm::Owned(Term::Literal(_))) => Err(IntoQuadsError::Graph),
				Some(CowRdfTerm::Borrowed(Term::Literal(_))) => Err(IntoQuadsError::Graph),
				None => Ok(interpretation.new_resource(vocabulary)),
			},
		}
	}

	fn object_as_subject<'a>(
		&self,
		object: &'a Self::Object,
	) -> Result<&'a Self::Subject, IntoQuadsError> {
		Ok(object)
	}

	fn subject_as_object<'a>(
		&self,
		subject: &'a Self::Subject,
	) -> Result<Self::ObjectRef<'a>, IntoQuadsError>
	where
		V::Iri: 'a,
		V::BlankId: 'a,
		V::Literal: 'a,
		I::Resource: 'a,
	{
		Ok(subject)
	}

	fn object_as_ref<'a>(object: &'a Self::Object) -> Self::ObjectRef<'a>
	where
		V::Iri: 'a,
		V::BlankId: 'a,
		V::Literal: 'a,
		I::Resource: 'a,
	{
		object
	}

	fn cloned_object_ref<'a>(object_ref: Self::ObjectRef<'a>) -> Self::Object
	where
		V::Iri: 'a,
		V::BlankId: 'a,
		V::Literal: 'a,
		I::Resource: 'a,
	{
		object_ref.clone()
	}
}

struct LexicalDomain;

fn lexical_term<V: Vocabulary>(vocabulary: &V, term: CowRdfTerm<V>) -> Term {
	match term {
		CowRdfTerm::Borrowed(Term::Id(Id::Iri(iri))) => {
			Term::Id(Id::Iri(vocabulary.iri(iri).unwrap().to_owned()))
		}
		CowRdfTerm::Owned(Term::Id(Id::Iri(iri))) => {
			Term::Id(Id::Iri(vocabulary.owned_iri(iri).ok().unwrap()))
		}
		CowRdfTerm::Borrowed(Term::Id(Id::Blank(blank_id))) => {
			Term::Id(Id::Blank(vocabulary.blank_id(blank_id).unwrap().to_owned()))
		}
		CowRdfTerm::Owned(Term::Id(Id::Blank(blank_id))) => {
			Term::Id(Id::Blank(vocabulary.owned_blank_id(blank_id).ok().unwrap()))
		}
		CowRdfTerm::Borrowed(Term::Literal(lit)) => Term::Literal(lit.into_lexical(vocabulary)),
		CowRdfTerm::Owned(Term::Literal(lit)) => Term::Literal(lit.into_lexical(vocabulary)),
	}
}

fn resource_lexical_term<V: Vocabulary, I>(
	vocabulary: &V,
	interpretation: &I,
	r: &I::Resource,
) -> Result<Term<Id, rdf_types::Literal>, IntoQuadsError>
where
	I: ReverseTermInterpretation<Iri = V::Iri, BlankId = V::BlankId, Literal = V::Literal>,
{
	if let Some(iri) = interpretation.iris_of(r).next() {
		let iri = vocabulary.iri(iri).unwrap();
		return Ok(Term::Id(Id::Iri(iri.to_owned())));
	}

	if let Some(lit) = interpretation.literals_of(r).next() {
		return Ok(Term::Literal(
			vocabulary
				.literal(lit)
				.unwrap()
				.extracted_from_vocabulary(vocabulary),
		));
	}

	if let Some(blank_id) = interpretation.blank_ids_of(r).next() {
		let blank_id = vocabulary.blank_id(blank_id).unwrap();
		return Ok(Term::Id(Id::Blank(blank_id.to_owned())));
	}

	Err(IntoQuadsError::MissingLexicalRepresentation)
}

impl<I: InterpretationMut<V>, V: Vocabulary> Domain<I, V> for LexicalDomain
where
	I: ReverseTermInterpretation<Iri = V::Iri, BlankId = V::BlankId, Literal = V::Literal>,
{
	type Subject = Id;
	type Predicate = IriBuf;
	type Object = Term;
	type ObjectRef<'a>
		= Term<&'a Id, &'a rdf_types::Literal>
	where
		V::Iri: 'a,
		V::BlankId: 'a,
		V::Literal: 'a,
		I::Resource: 'a;

	fn subject(
		&mut self,
		vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<I, V>,
	) -> Result<Self::Subject, IntoQuadsError> {
		match value {
			ResourceInterpretation::Interpreted(r) => {
				if let Some(iri) = interpretation.iris_of(r).next() {
					let iri = vocabulary.iri(iri).unwrap();
					return Ok(Id::Iri(iri.to_owned()));
				}

				if let Some(blank_id) = interpretation.blank_ids_of(r).next() {
					let blank_id = vocabulary.blank_id(blank_id).unwrap();
					return Ok(Id::Blank(blank_id.to_owned()));
				}

				Err(IntoQuadsError::MissingLexicalRepresentation)
			}
			ResourceInterpretation::Uninterpreted(u) => u
				.map(|t| Ok(lexical_term(vocabulary, t)))
				.unwrap_or_else(|| {
					let r = interpretation.new_resource(vocabulary);
					resource_lexical_term(vocabulary, interpretation, &r)
				})?
				.into_id()
				.ok_or(IntoQuadsError::Subject),
		}
	}

	fn predicate(
		&mut self,
		vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<I, V>,
	) -> Result<Self::Predicate, IntoQuadsError> {
		match value {
			ResourceInterpretation::Interpreted(r) => {
				if let Some(iri) = interpretation.iris_of(r).next() {
					let iri = vocabulary.iri(iri).unwrap();
					return Ok(iri.to_owned());
				}

				Err(IntoQuadsError::Predicate)
			}
			ResourceInterpretation::Uninterpreted(u) => match u {
				Some(CowRdfTerm::Owned(Term::Id(Id::Iri(iri)))) => {
					Ok(vocabulary.owned_iri(iri).ok().unwrap())
				}
				Some(CowRdfTerm::Borrowed(Term::Id(Id::Iri(iri)))) => {
					Ok(vocabulary.iri(iri).unwrap().to_owned())
				}
				_ => Err(IntoQuadsError::Predicate),
			},
		}
	}

	fn object(
		&mut self,
		vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<I, V>,
	) -> Result<Self::Object, IntoQuadsError> {
		match value {
			ResourceInterpretation::Interpreted(r) => {
				resource_lexical_term(vocabulary, interpretation, r)
			}
			ResourceInterpretation::Uninterpreted(u) => {
				let term = match u {
					Some(CowRdfTerm::Owned(Term::Id(Id::Iri(iri)))) => {
						Term::Id(Id::Iri(vocabulary.owned_iri(iri).ok().unwrap()))
					}
					Some(CowRdfTerm::Borrowed(Term::Id(Id::Iri(iri)))) => {
						Term::Id(Id::Iri(vocabulary.iri(iri).unwrap().to_owned()))
					}
					Some(CowRdfTerm::Owned(Term::Id(Id::Blank(blank_id)))) => {
						Term::Id(Id::Blank(vocabulary.owned_blank_id(blank_id).ok().unwrap()))
					}
					Some(CowRdfTerm::Borrowed(Term::Id(Id::Blank(blank_id)))) => {
						Term::Id(Id::Blank(vocabulary.blank_id(blank_id).unwrap().to_owned()))
					}
					Some(CowRdfTerm::Owned(Term::Literal(lit))) => {
						Term::Literal(lit.into_lexical(vocabulary))
					}
					Some(CowRdfTerm::Borrowed(Term::Literal(lit))) => {
						Term::Literal(lit.into_lexical(vocabulary))
					}
					None => {
						let r = interpretation.new_resource(vocabulary);
						resource_lexical_term(vocabulary, interpretation, &r)?
					}
				};

				Ok(term)
			}
		}
	}

	fn graph(
		&mut self,
		vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<I, V>,
	) -> Result<Self::Subject, IntoQuadsError> {
		match value {
			ResourceInterpretation::Interpreted(r) => {
				if let Some(iri) = interpretation.iris_of(r).next() {
					let iri = vocabulary.iri(iri).unwrap();
					return Ok(Id::Iri(iri.to_owned()));
				}

				if let Some(blank_id) = interpretation.blank_ids_of(r).next() {
					let blank_id = vocabulary.blank_id(blank_id).unwrap();
					return Ok(Id::Blank(blank_id.to_owned()));
				}

				Err(IntoQuadsError::MissingLexicalRepresentation)
			}
			ResourceInterpretation::Uninterpreted(u) => u
				.map(|t| Ok(lexical_term(vocabulary, t)))
				.unwrap_or_else(|| {
					let r = interpretation.new_resource(vocabulary);
					resource_lexical_term(vocabulary, interpretation, &r)
				})?
				.into_id()
				.ok_or(IntoQuadsError::Graph),
		}
	}

	fn object_as_subject<'a>(
		&self,
		object: &'a Self::Object,
	) -> Result<&'a Self::Subject, IntoQuadsError> {
		match object {
			Term::Id(id) => Ok(id),
			Term::Literal(_) => Err(IntoQuadsError::Subject),
		}
	}

	fn subject_as_object<'a>(
		&self,
		subject: &'a Self::Subject,
	) -> Result<Self::ObjectRef<'a>, IntoQuadsError>
	where
		V::Iri: 'a,
		V::BlankId: 'a,
		V::Literal: 'a,
		I::Resource: 'a,
	{
		Ok(Term::Id(subject))
	}

	fn object_as_ref<'a>(object: &'a Self::Object) -> Self::ObjectRef<'a>
	where
		V::Iri: 'a,
		V::BlankId: 'a,
		V::Literal: 'a,
		I::Resource: 'a,
	{
		object.as_ref()
	}

	fn cloned_object_ref<'a>(object_ref: Self::ObjectRef<'a>) -> Self::Object
	where
		V::Iri: 'a,
		V::BlankId: 'a,
		V::Literal: 'a,
		I::Resource: 'a,
	{
		object_ref.cloned()
	}
}

/// A simple serializer generating a list of `Quad`s.
struct QuadSerializer<'a, I: Interpretation, V: Vocabulary, D: Domain<I, V>> {
	vocabulary: &'a mut V,
	interpretation: &'a mut I,
	domain: &'a mut D,
	result: Vec<DomainQuad<I, V, D>>,
}

impl<'a, I: Interpretation, V: Vocabulary, D: Domain<I, V>> Visitor<I, V>
	for QuadSerializer<'a, I, V, D>
{
	type Ok = Vec<DomainQuad<I, V, D>>;
	type Error = IntoQuadsError;

	fn default_graph<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + crate::LinkedDataGraph<I, V>,
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
		T: ?Sized + LinkedDataResource<I, V> + crate::LinkedDataGraph<I, V>,
	{
		let i = value.interpretation(self.vocabulary, self.interpretation);
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

struct QuadGraphSerializer<'a, I: Interpretation, V: Vocabulary, D: Domain<I, V>> {
	vocabulary: &'a mut V,
	interpretation: &'a mut I,
	domain: &'a mut D,
	result: &'a mut Vec<DomainQuad<I, V, D>>,
	graph: Option<&'a D::Subject>,
}

impl<'a, I: Interpretation, V: Vocabulary, D: Domain<I, V>> GraphVisitor<I, V>
	for QuadGraphSerializer<'a, I, V, D>
{
	type Ok = ();
	type Error = IntoQuadsError;

	fn subject<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataResource<I, V> + crate::LinkedDataSubject<I, V>,
	{
		let i = value.interpretation(self.vocabulary, self.interpretation);
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

struct QuadPropertiesSerializer<'a, I: Interpretation, V: Vocabulary, D: Domain<I, V>> {
	vocabulary: &'a mut V,
	interpretation: &'a mut I,
	domain: &'a mut D,
	result: &'a mut Vec<DomainQuad<I, V, D>>,
	graph: Option<&'a D::Subject>,
	subject: SubjectOrObject<'a, I, V, D>,
}

#[derive(Educe)]
#[educe(Clone, Copy)]
enum SubjectOrObject<'a, I: Interpretation, V: Vocabulary, D: Domain<I, V>> {
	Subject(&'a D::Subject),
	Object(&'a D::Object),
}

impl<'a, I: Interpretation, V: Vocabulary, D: Domain<I, V>> SubjectOrObject<'a, I, V, D> {
	fn into_subject(self, domain: &D) -> Result<&'a D::Subject, IntoQuadsError> {
		match self {
			Self::Subject(s) => Ok(s),
			Self::Object(o) => domain.object_as_subject(o),
		}
	}

	fn into_object(self, domain: &D) -> Result<D::ObjectRef<'a>, IntoQuadsError> {
		match self {
			Self::Subject(s) => domain.subject_as_object(s),
			Self::Object(o) => Ok(D::object_as_ref(o)),
		}
	}
}

impl<'a, I: Interpretation, V: Vocabulary, D: Domain<I, V>> SubjectVisitor<I, V>
	for QuadPropertiesSerializer<'a, I, V, D>
{
	type Ok = ();
	type Error = IntoQuadsError;

	fn predicate<L, T>(&mut self, predicate: &L, value: &T) -> Result<(), Self::Error>
	where
		L: ?Sized + LinkedDataResource<I, V>,
		T: ?Sized + crate::LinkedDataPredicateObjects<I, V>,
	{
		let subject = self.subject.into_subject(self.domain)?;

		let i = predicate.interpretation(self.vocabulary, self.interpretation);
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

	fn reverse_predicate<L, T>(&mut self, predicate: &L, subjects: &T) -> Result<(), Self::Error>
	where
		L: ?Sized + LinkedDataResource<I, V>,
		T: ?Sized + crate::LinkedDataPredicateObjects<I, V>,
	{
		let object = self.subject.into_object(self.domain)?;

		let i = predicate.interpretation(self.vocabulary, self.interpretation);
		let term = self
			.domain
			.predicate(self.vocabulary, self.interpretation, i)?;

		let subjects_serializer = ReversePredicateSerializer {
			vocabulary: self.vocabulary,
			interpretation: self.interpretation,
			domain: self.domain,
			result: self.result,
			graph: self.graph,
			object,
			predicate: term,
		};

		subjects.visit_objects(subjects_serializer)
	}

	fn graph<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataGraph<I, V>,
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

	fn include<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataResource<I, V> + LinkedDataSubject<I, V>,
	{
		let i = value.interpretation(self.vocabulary, self.interpretation);
		let subject = self
			.domain
			.subject(self.vocabulary, self.interpretation, i)?;

		value.visit_subject(QuadPropertiesSerializer {
			vocabulary: self.vocabulary,
			interpretation: self.interpretation,
			domain: self.domain,
			graph: self.graph,
			subject: SubjectOrObject::Subject(&subject),
			result: self.result,
		})?;

		Ok(())
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(())
	}
}

struct ObjectsSerializer<'a, I: Interpretation, V: Vocabulary, D: Domain<I, V>> {
	vocabulary: &'a mut V,
	interpretation: &'a mut I,
	domain: &'a mut D,
	result: &'a mut Vec<DomainQuad<I, V, D>>,
	graph: Option<&'a D::Subject>,
	subject: &'a D::Subject,
	predicate: D::Predicate,
}

impl<'a, I: Interpretation, V: Vocabulary, D: Domain<I, V>> PredicateObjectsVisitor<I, V>
	for ObjectsSerializer<'a, I, V, D>
{
	type Ok = ();
	type Error = IntoQuadsError;

	fn object<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataResource<I, V> + crate::LinkedDataSubject<I, V>,
	{
		let i = value.interpretation(self.vocabulary, self.interpretation);
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

struct ReversePredicateSerializer<'a, I: Interpretation, V: Vocabulary, D: Domain<I, V>> {
	vocabulary: &'a mut V,
	interpretation: &'a mut I,
	domain: &'a mut D,
	result: &'a mut Vec<DomainQuad<I, V, D>>,
	graph: Option<&'a D::Subject>,
	object: D::ObjectRef<'a>,
	predicate: D::Predicate,
}

impl<'a, I: Interpretation, V: Vocabulary, D: Domain<I, V>> PredicateObjectsVisitor<I, V>
	for ReversePredicateSerializer<'a, I, V, D>
{
	type Ok = ();
	type Error = IntoQuadsError;

	fn object<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataResource<I, V> + crate::LinkedDataSubject<I, V>,
	{
		let i = value.interpretation(self.vocabulary, self.interpretation);
		let subject = self
			.domain
			.subject(self.vocabulary, self.interpretation, i)?;

		let subject_serializer = QuadPropertiesSerializer {
			vocabulary: self.vocabulary,
			interpretation: self.interpretation,
			domain: self.domain,
			result: self.result,
			graph: self.graph,
			subject: SubjectOrObject::Subject(&subject),
		};

		value.visit_subject(subject_serializer)?;
		self.result.push(Quad(
			subject,
			self.predicate.clone(),
			D::cloned_object_ref(self.object),
			self.graph.cloned(),
		));
		Ok(())
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(())
	}
}
