use educe::Educe;
use iref::IriBuf;
use rdf_types::{
	interpretation::{ReverseBlankIdInterpretation, ReverseIriInterpretation},
	BlankIdInterpretationMut, ExportedFromVocabulary, Generator, Id, InsertIntoVocabulary,
	Interpretation, InterpretationMut, IriInterpretationMut, IriVocabularyMut,
	LiteralInterpretationMut, LiteralVocabularyMut, Quad, ReverseLiteralInterpretation, Term,
	Vocabulary,
};

use crate::{
	CowRdfTerm, GraphVisitor, InterpretedQuad, LinkedData, LinkedDataGraph, LinkedDataResource,
	LinkedDataSubject, PredicateObjectsVisitor, RdfId, RdfLiteralType, RdfLiteralValue, RdfQuad,
	ResourceInterpretation, SubjectVisitor, Visitor,
};

pub fn to_interpreted_quads<V: Vocabulary, I: Interpretation>(
	vocabulary: &mut V,
	interpretation: &mut I,
	value: &impl LinkedData<V, I>,
) -> Result<Vec<InterpretedQuad<I>>, IntoQuadsError>
where
	I: InterpretationMut
		+ IriInterpretationMut<V::Iri>
		+ BlankIdInterpretationMut<V::BlankId>
		+ LiteralInterpretationMut<V::Literal>,
	I::Resource: Clone,
	V: IriVocabularyMut + LiteralVocabularyMut,
	V::Iri: Clone,
	V::BlankId: Clone,
	V::Value: RdfLiteralValue,
	V::Type: RdfLiteralType<V>,
	V::LanguageTag: Clone,
{
	value.visit(QuadSerializer {
		vocabulary,
		interpretation,
		domain: &mut InterpretationDomain,
		result: Vec::new(),
	})
}

pub fn to_interpreted_subject_quads<V: Vocabulary, I: Interpretation>(
	vocabulary: &mut V,
	interpretation: &mut I,
	graph: Option<&I::Resource>,
	value: &(impl LinkedDataSubject<V, I> + LinkedDataResource<V, I>),
) -> Result<(I::Resource, Vec<InterpretedQuad<I>>), IntoQuadsError>
where
	I: InterpretationMut
		+ IriInterpretationMut<V::Iri>
		+ BlankIdInterpretationMut<V::BlankId>
		+ LiteralInterpretationMut<V::Literal>,
	I::Resource: Clone,
	V: IriVocabularyMut + LiteralVocabularyMut,
	V::Iri: Clone,
	V::BlankId: Clone,
	V::Value: RdfLiteralValue,
	V::Type: RdfLiteralType<V>,
	V::LanguageTag: Clone,
{
	let mut result = Vec::new();

	let subject = match value.interpretation(vocabulary, interpretation) {
		ResourceInterpretation::Interpreted(r) => r.clone(),
		ResourceInterpretation::Uninterpreted(_) => interpretation.new_resource(),
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

pub fn to_lexical_quads_with<V: Vocabulary, I: Interpretation>(
	vocabulary: &mut V,
	interpretation: &mut I,
	generator: impl Generator<()>,
	value: &impl LinkedData<V, I>,
) -> Result<Vec<Quad>, IntoQuadsError>
where
	I: ReverseIriInterpretation<Iri = V::Iri>
		+ ReverseBlankIdInterpretation<BlankId = V::BlankId>
		+ ReverseLiteralInterpretation<Literal = V::Literal>,
	V::Literal: ExportedFromVocabulary<V, Output = rdf_types::Literal>,
{
	let mut domain = LexicalDomain { generator };

	value.visit(QuadSerializer {
		vocabulary,
		interpretation,
		domain: &mut domain,
		result: Vec::new(),
	})
}

pub fn to_lexical_quads(
	generator: impl Generator<()>,
	value: &impl LinkedData,
) -> Result<Vec<Quad>, IntoQuadsError> {
	to_lexical_quads_with(&mut (), &mut (), generator, value)
}

pub fn to_quads_with<V: Vocabulary, I: Interpretation>(
	vocabulary: &mut V,
	interpretation: &mut I,
	generator: impl Generator<V>,
	value: &impl LinkedData<V, I>,
) -> Result<Vec<RdfQuad<V>>, IntoQuadsError>
where
	V: IriVocabularyMut + LiteralVocabularyMut,
	V::BlankId: Clone,
	V::Iri: Clone,
	V::Literal: Clone,
	V::Value: RdfLiteralValue,
	V::Type: RdfLiteralType<V>,
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
) -> Result<Vec<Quad>, IntoQuadsError> {
	let mut domain = VocabularyDomain { generator };

	value.visit(QuadSerializer {
		vocabulary: &mut (),
		interpretation: &mut (),
		domain: &mut domain,
		result: Vec::new(),
	})
}

#[derive(Debug, thiserror::Error)]
pub enum IntoQuadsError {
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
		value: ResourceInterpretation<V, I>,
	) -> Result<Self::Subject, IntoQuadsError>;

	fn predicate(
		&mut self,
		vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<V, I>,
	) -> Result<Self::Predicate, IntoQuadsError>;

	fn object(
		&mut self,
		vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<V, I>,
	) -> Result<Self::Object, IntoQuadsError>;

	fn graph(
		&mut self,
		vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<V, I>,
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
	V::Value: RdfLiteralValue,
	V::Type: RdfLiteralType<V>,
	V::LanguageTag: Clone,
	I: ReverseIriInterpretation<Iri = V::Iri>
		+ ReverseBlankIdInterpretation<BlankId = V::BlankId>
		+ ReverseLiteralInterpretation<Literal = V::Literal>,
{
	type Subject = RdfId<V>;
	type Predicate = V::Iri;
	type Object = Term<RdfId<V>, V::Literal>;
	type ObjectRef<'a> = Term<&'a RdfId<V>, &'a V::Literal> where V::Iri: 'a, V::BlankId: 'a, V::Literal: 'a, I::Resource: 'a;

	fn subject(
		&mut self,
		vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<V, I>,
	) -> Result<Self::Subject, IntoQuadsError> {
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
				.ok_or(IntoQuadsError::Subject),
		}
	}

	fn predicate(
		&mut self,
		_vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<V, I>,
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
		value: ResourceInterpretation<V, I>,
	) -> Result<Self::Object, IntoQuadsError> {
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
	) -> Result<Self::Subject, IntoQuadsError> {
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
	V::Value: RdfLiteralValue,
	V::Type: RdfLiteralType<V>,
	V::LanguageTag: Clone,
{
	type Subject = I::Resource;
	type Predicate = I::Resource;
	type Object = I::Resource;
	type ObjectRef<'a> = &'a I::Resource where V::Iri: 'a, V::BlankId: 'a, V::Literal: 'a, I::Resource: 'a;

	fn subject(
		&mut self,
		_vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<V, I>,
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
				None => Ok(interpretation.new_resource()),
			},
		}
	}

	fn predicate(
		&mut self,
		_vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<V, I>,
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
		value: ResourceInterpretation<V, I>,
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
				None => Ok(interpretation.new_resource()),
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

struct LexicalDomain<G> {
	generator: G,
}

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

impl<V: Vocabulary, I: Interpretation, G: Generator<()>> Domain<V, I> for LexicalDomain<G>
where
	I: ReverseIriInterpretation<Iri = V::Iri>
		+ ReverseBlankIdInterpretation<BlankId = V::BlankId>
		+ ReverseLiteralInterpretation<Literal = V::Literal>,
	V::Literal: ExportedFromVocabulary<V, Output = rdf_types::Literal>,
{
	type Subject = Id;
	type Predicate = IriBuf;
	type Object = Term;
	type ObjectRef<'a> = Term<&'a Id, &'a rdf_types::Literal> where V::Iri: 'a, V::BlankId: 'a, V::Literal: 'a, I::Resource: 'a;

	fn subject(
		&mut self,
		vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<V, I>,
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

				Ok(self.generator.next(&mut ()))
			}
			ResourceInterpretation::Uninterpreted(u) => u
				.map(|t| lexical_term(vocabulary, t))
				.unwrap_or_else(|| Term::Id(self.generator.next(&mut ())))
				.into_id()
				.ok_or(IntoQuadsError::Subject),
		}
	}

	fn predicate(
		&mut self,
		vocabulary: &mut V,
		interpretation: &mut I,
		value: ResourceInterpretation<V, I>,
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
		value: ResourceInterpretation<V, I>,
	) -> Result<Self::Object, IntoQuadsError> {
		match value {
			ResourceInterpretation::Interpreted(r) => {
				if let Some(iri) = interpretation.iris_of(r).next() {
					let iri = vocabulary.iri(iri).unwrap();
					return Ok(Term::Id(Id::Iri(iri.to_owned())));
				}

				if let Some(lit) = interpretation.literals_of(r).next() {
					return Ok(Term::Literal(lit.exported_from_vocabulary(vocabulary)));
				}

				if let Some(blank_id) = interpretation.blank_ids_of(r).next() {
					let blank_id = vocabulary.blank_id(blank_id).unwrap();
					return Ok(Term::Id(Id::Blank(blank_id.to_owned())));
				}

				Ok(Term::Id(self.generator.next(&mut ())))
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
					None => Term::Id(self.generator.next(&mut ())),
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

				Ok(self.generator.next(&mut ()))
			}
			ResourceInterpretation::Uninterpreted(u) => u
				.map(|t| lexical_term(vocabulary, t))
				.unwrap_or_else(|| Term::Id(self.generator.next(&mut ())))
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
	type Error = IntoQuadsError;

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
		T: ?Sized + LinkedDataResource<V, I> + crate::LinkedDataGraph<V, I>,
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
	type Error = IntoQuadsError;

	fn subject<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataResource<V, I> + crate::LinkedDataSubject<V, I>,
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

impl<'a, V: Vocabulary, I: Interpretation, D: Domain<V, I>> SubjectVisitor<V, I>
	for QuadPropertiesSerializer<'a, V, I, D>
{
	type Ok = ();
	type Error = IntoQuadsError;

	fn predicate<L, T>(&mut self, predicate: &L, value: &T) -> Result<(), Self::Error>
	where
		L: ?Sized + LinkedDataResource<V, I>,
		T: ?Sized + crate::LinkedDataPredicateObjects<V, I>,
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
		L: ?Sized + LinkedDataResource<V, I>,
		T: ?Sized + crate::LinkedDataPredicateObjects<V, I>,
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

	fn include<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataResource<V, I> + LinkedDataSubject<V, I>,
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
	type Error = IntoQuadsError;

	fn object<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataResource<V, I> + crate::LinkedDataSubject<V, I>,
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

struct ReversePredicateSerializer<'a, V: Vocabulary, I: Interpretation, D: Domain<V, I>> {
	vocabulary: &'a mut V,
	interpretation: &'a mut I,
	domain: &'a mut D,
	result: &'a mut Vec<DomainQuad<V, I, D>>,
	graph: Option<&'a D::Subject>,
	object: D::ObjectRef<'a>,
	predicate: D::Predicate,
}

impl<'a, V: Vocabulary, I: Interpretation, D: Domain<V, I>> PredicateObjectsVisitor<V, I>
	for ReversePredicateSerializer<'a, V, I, D>
{
	type Ok = ();
	type Error = IntoQuadsError;

	fn object<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataResource<V, I> + crate::LinkedDataSubject<V, I>,
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
