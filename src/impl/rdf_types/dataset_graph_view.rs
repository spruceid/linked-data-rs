use rdf_types::{
	dataset::{DatasetGraphView, PatternMatchingDataset, PredicateTraversableDataset},
	Dataset, Interpretation, Vocabulary,
};
use std::hash::Hash;

use crate::{
	LinkedDataPredicateObjects, LinkedDataResource, LinkedDataSubject, PredicateObjectsVisitor,
	ResourceInterpretation, SubjectVisitor,
};

impl<'a, I: Interpretation, V: Vocabulary, D> LinkedDataSubject<I, V> for DatasetGraphView<'a, D>
where
	I::Resource: Eq + Hash + LinkedDataResource<I, V>,
	D: PredicateTraversableDataset<Resource = I::Resource> + PatternMatchingDataset,
{
	fn visit_subject<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<I, V>,
	{
		let mut visited = im::HashSet::new();
		visited.insert(self.resource);

		Subject::new(self.dataset, self.graph, self.resource, &visited, true)
			.visit(&mut serializer)?;
		serializer.end()
	}
}

struct PredicateObjects<'d, 'v, D: Dataset> {
	dataset: &'d D,
	graph: Option<&'d D::Resource>,
	subject: &'d D::Resource,
	predicate: &'d D::Resource,
	visited: &'v im::HashSet<&'d D::Resource>,
}

impl<'d, 'v, I: Interpretation, V: Vocabulary, D> LinkedDataPredicateObjects<I, V>
	for PredicateObjects<'d, 'v, D>
where
	I::Resource: Eq + Hash + LinkedDataResource<I, V>,
	D: PredicateTraversableDataset<Resource = I::Resource> + PatternMatchingDataset,
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
				visited: self.visited,
			})?;
		}

		visitor.end()
	}
}

impl<'a, 'v, I: Interpretation, V: Vocabulary, D: Dataset<Resource = I::Resource>>
	LinkedDataResource<I, V> for Object<'a, 'v, D>
where
	I::Resource: LinkedDataResource<I, V>,
{
	fn interpretation(
		&self,
		vocabulary: &mut V,
		interpretation: &mut I,
	) -> ResourceInterpretation<I, V> {
		self.object.interpretation(vocabulary, interpretation)
	}
}

struct Object<'d, 'v, D: Dataset> {
	dataset: &'d D,
	graph: Option<&'d D::Resource>,
	object: &'d D::Resource,
	visited: &'v im::HashSet<&'d D::Resource>,
}

impl<'d, 'v, I: Interpretation, V: Vocabulary, D> LinkedDataSubject<I, V> for Object<'d, 'v, D>
where
	I::Resource: Eq + Hash + LinkedDataResource<I, V>,
	D: PredicateTraversableDataset<Resource = I::Resource> + PatternMatchingDataset,
{
	fn visit_subject<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<I, V>,
	{
		let subject = self.object;
		let mut visited = self.visited.clone();
		let visit_predicates = visited.insert(subject).is_none();

		Subject::new(
			self.dataset,
			self.graph,
			subject,
			&visited,
			visit_predicates,
		)
		.visit(&mut visitor)?;

		visitor.end()
	}
}

struct Subject<'d, 'v, D: Dataset> {
	dataset: &'d D,
	graph: Option<&'d D::Resource>,
	subject: &'d D::Resource,
	visited: &'v im::HashSet<&'d D::Resource>,
	visit_predicates: bool,
}

impl<'d, 'v, D: PredicateTraversableDataset + PatternMatchingDataset> Subject<'d, 'v, D> {
	fn new(
		dataset: &'d D,
		graph: Option<&'d D::Resource>,
		subject: &'d D::Resource,
		visited: &'v im::HashSet<&'d D::Resource>,
		visit_predicates: bool,
	) -> Self {
		Self {
			dataset,
			graph,
			subject,
			visited,
			visit_predicates,
		}
	}

	fn visit<I: Interpretation<Resource = D::Resource>, V: Vocabulary, S>(
		&self,
		visitor: &mut S,
	) -> Result<(), S::Error>
	where
		S: SubjectVisitor<I, V>,
		I::Resource: Eq + Hash + LinkedDataResource<I, V>,
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
					visited: self.visited,
				},
			)?;
		}

		Ok(())
	}
}

impl<'d, 'v, I: Interpretation, V: Vocabulary, D> LinkedDataSubject<I, V> for Subject<'d, 'v, D>
where
	D::Resource: Eq + Hash + LinkedDataResource<I, V>,
	D: PredicateTraversableDataset<Resource = I::Resource> + PatternMatchingDataset,
{
	fn visit_subject<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<I, V>,
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
						visited: self.visited,
					},
				)?;
			}
		}

		visitor.end()
	}
}
