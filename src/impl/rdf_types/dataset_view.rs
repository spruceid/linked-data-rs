use std::hash::Hash;

use crate::{
	GraphVisitor, LinkedDataGraph, LinkedDataPredicateObjects, LinkedDataResource,
	LinkedDataSubject, PredicateObjectsVisitor, ResourceInterpretation, SubjectVisitor,
};
use rdf_types::{
	dataset::{
		DatasetView, PatternMatchingDataset, PredicateTraversableDataset, SubjectTraversableDataset,
	},
	Dataset, Interpretation, Quad, Vocabulary,
};

impl<'a, I: Interpretation, V: Vocabulary, D> LinkedDataGraph<I, V> for DatasetView<'a, D>
where
	I::Resource: Eq + Hash + LinkedDataResource<I, V>,
	D: SubjectTraversableDataset<Resource = I::Resource>
		+ PredicateTraversableDataset
		+ PatternMatchingDataset,
{
	fn visit_graph<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<I, V>,
	{
		let mut visited_subjects = im::HashSet::new();
		let mut visited_graphs = im::HashSet::new();
		if let Some(g) = self.graph {
			visited_graphs.insert(g);
		}

		let mut graph_subjects = Vec::new();

		for subject in self.dataset.subjects() {
			if self.graph.is_none()
				|| self
					.dataset
					.quad_pattern_matching(Quad(Some(subject), None, None, Some(self.graph)).into())
					.next()
					.is_some()
			{
				visited_subjects.insert(subject);
				graph_subjects.push(subject);
			}
		}

		for subject in graph_subjects {
			visitor.subject(&Subject::new(
				self.dataset,
				self.graph,
				subject,
				&visited_subjects,
				&visited_graphs,
				true,
			))?;
		}

		visitor.end()
	}
}

struct PredicateObjects<'d, 'v, D: Dataset> {
	dataset: &'d D,
	graph: Option<&'d D::Resource>,
	subject: &'d D::Resource,
	predicate: &'d D::Resource,
	visited_subjects: &'v im::HashSet<&'d D::Resource>,
	visited_graphs: &'v im::HashSet<&'d D::Resource>,
}

impl<'d, 'v, I: Interpretation, V: Vocabulary, D> LinkedDataPredicateObjects<I, V>
	for PredicateObjects<'d, 'v, D>
where
	I::Resource: Eq + Hash + LinkedDataResource<I, V>,
	D: SubjectTraversableDataset<Resource = I::Resource>
		+ PredicateTraversableDataset
		+ PatternMatchingDataset,
{
	fn visit_objects<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<I, V>,
	{
		for object in self
			.dataset
			.quad_objects(self.graph, self.subject, self.predicate)
		{
			visitor.object(&Object {
				dataset: self.dataset,
				graph: self.graph,
				object,
				visited_subjects: self.visited_subjects,
				visited_graphs: self.visited_graphs,
			})?;
		}

		visitor.end()
	}
}

impl<'a, 'v, D, I: Interpretation, V: Vocabulary> LinkedDataResource<I, V> for Object<'a, 'v, D>
where
	I::Resource: LinkedDataResource<I, V>,
	D: Dataset<Resource = I::Resource>,
{
	fn interpretation(
		&self,
		vocabulary: &mut V,
		interpretation: &mut I,
	) -> ResourceInterpretation<I, V> {
		self.object.interpretation(vocabulary, interpretation)
	}
}

struct Object<'a, 'v, D: Dataset> {
	dataset: &'a D,
	graph: Option<&'a D::Resource>,
	object: &'a D::Resource,
	visited_subjects: &'v im::HashSet<&'a D::Resource>,
	visited_graphs: &'v im::HashSet<&'a D::Resource>,
}

impl<'a, 'v, I: Interpretation, V: Vocabulary, D> LinkedDataSubject<I, V> for Object<'a, 'v, D>
where
	I::Resource: Eq + Hash + LinkedDataResource<I, V>,
	D: SubjectTraversableDataset
		+ PredicateTraversableDataset<Resource = I::Resource>
		+ PatternMatchingDataset,
{
	fn visit_subject<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<I, V>,
	{
		let subject = self.object;

		let mut visited_subjects = self.visited_subjects.clone();
		let visit_predicates = visited_subjects.insert(subject).is_none();

		Subject::new(
			self.dataset,
			self.graph,
			subject,
			&visited_subjects,
			self.visited_graphs,
			visit_predicates,
		)
		.visit(&mut visitor)?;

		visitor.end()
	}
}

struct Subject<'a, 'v, D: Dataset> {
	dataset: &'a D,
	graph: Option<&'a D::Resource>,
	subject: &'a D::Resource,
	visited_subjects: &'v im::HashSet<&'a D::Resource>,
	visited_graphs: &'v im::HashSet<&'a D::Resource>,
	visit_predicates: bool,
}

impl<
		'a,
		'v,
		D: SubjectTraversableDataset + PredicateTraversableDataset + PatternMatchingDataset,
	> Subject<'a, 'v, D>
where
	D::Resource: Eq + Hash,
{
	fn new(
		dataset: &'a D,
		graph: Option<&'a D::Resource>,
		subject: &'a D::Resource,
		visited_subjects: &'v im::HashSet<&'a D::Resource>,
		visited_graphs: &'v im::HashSet<&'a D::Resource>,
		visit_predicates: bool,
	) -> Self {
		Self {
			dataset,
			graph,
			subject,
			visited_subjects,
			visited_graphs,
			visit_predicates,
		}
	}

	fn visit<I: Interpretation<Resource = D::Resource>, V: Vocabulary, S>(
		&self,
		visitor: &mut S,
	) -> Result<(), S::Error>
	where
		S: SubjectVisitor<I, V>,
		D::Resource: LinkedDataResource<I, V>,
	{
		if self.visit_predicates {
			for (predicate, _) in self
				.dataset
				.quad_predicates_objects(self.graph, self.subject)
			{
				visitor.predicate(
					predicate,
					&PredicateObjects {
						dataset: self.dataset,
						graph: self.graph,
						subject: self.subject,
						predicate,
						visited_subjects: self.visited_subjects,
						visited_graphs: self.visited_graphs,
					},
				)?;
			}

			if self.dataset.contains_named_graph(self.subject) {
				let mut visited_graphs = self.visited_graphs.clone();
				if visited_graphs.insert(self.subject).is_none() {
					visitor.graph(&NamedGraphView {
						dataset: self.dataset,
						graph: self.subject,
						visited_graphs: &visited_graphs,
					})?;
				}
			}
		}

		Ok(())
	}
}

impl<'a, 'v, I: Interpretation, V: Vocabulary, D> LinkedDataResource<I, V> for Subject<'a, 'v, D>
where
	I::Resource: LinkedDataResource<I, V>,
	D: Dataset<Resource = I::Resource>,
{
	fn interpretation(
		&self,
		vocabulary: &mut V,
		interpretation: &mut I,
	) -> ResourceInterpretation<I, V> {
		self.subject.interpretation(vocabulary, interpretation)
	}
}

impl<'a, 'v, I: Interpretation, V: Vocabulary, D> LinkedDataSubject<I, V> for Subject<'a, 'v, D>
where
	I::Resource: Eq + Hash + LinkedDataResource<I, V>,
	D: SubjectTraversableDataset<Resource = I::Resource>
		+ PredicateTraversableDataset
		+ PatternMatchingDataset,
{
	fn visit_subject<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<I, V>,
	{
		for (predicate, _) in self
			.dataset
			.quad_predicates_objects(self.graph, self.subject)
		{
			visitor.predicate(
				predicate,
				&PredicateObjects {
					dataset: self.dataset,
					graph: self.graph,
					subject: self.subject,
					predicate,
					visited_subjects: self.visited_subjects,
					visited_graphs: self.visited_graphs,
				},
			)?;
		}

		visitor.end()
	}
}

struct NamedGraphView<'a, 'v, D: Dataset> {
	dataset: &'a D,
	graph: &'a D::Resource,
	visited_graphs: &'v im::HashSet<&'a D::Resource>,
}

impl<'a, 'v, I: Interpretation, V: Vocabulary, D> LinkedDataResource<I, V>
	for NamedGraphView<'a, 'v, D>
where
	I::Resource: LinkedDataResource<I, V>,
	D: Dataset<Resource = I::Resource>,
{
	fn interpretation(
		&self,
		vocabulary: &mut V,
		interpretation: &mut I,
	) -> ResourceInterpretation<I, V> {
		self.graph.interpretation(vocabulary, interpretation)
	}
}

impl<'a, 'v, I: Interpretation, V: Vocabulary, D> LinkedDataGraph<I, V>
	for NamedGraphView<'a, 'v, D>
where
	I::Resource: Eq + Hash + LinkedDataResource<I, V>,
	D: SubjectTraversableDataset<Resource = I::Resource>
		+ PredicateTraversableDataset
		+ PatternMatchingDataset,
{
	fn visit_graph<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: GraphVisitor<I, V>,
	{
		let mut visited_subjects = im::HashSet::new();
		let mut graph_subjects = Vec::new();

		for subject in self.dataset.subjects() {
			if self
				.dataset
				.quad_pattern_matching(
					Quad(Some(subject), None, None, Some(Some(self.graph))).into(),
				)
				.next()
				.is_some()
			{
				visited_subjects.insert(subject);
				graph_subjects.push(subject);
			}
		}

		for subject in graph_subjects {
			visitor.subject(&Subject::new(
				self.dataset,
				Some(self.graph),
				subject,
				&visited_subjects,
				self.visited_graphs,
				true,
			))?;
		}

		visitor.end()
	}
}
