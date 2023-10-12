use std::hash::Hash;
use grdf::{Graph, GraphAccess, GraphView};
use rdf_types::{Vocabulary, Interpretation};

use crate::{
	LinkedDataResource, LinkedDataPredicateObjects, LinkedDataSubject, PredicateObjectsVisitor,
	ResourceInterpretation, SubjectVisitor,
};

impl<'a, G: ?Sized + Graph, A: GraphAccess<G>, V: Vocabulary, I: Interpretation>
	LinkedDataSubject<V, I> for GraphView<'a, G, A>
where
	G::Subject: Eq + Hash,
	G::Predicate: LinkedDataResource<V, I>,
	G::Object: LinkedDataResource<V, I>,
{
	fn visit_subject<S>(&self, mut serializer: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<V, I>,
	{
		let mut visited = im::HashSet::new();
		visited.insert(self.subject);

		Subject::new(self.graph, self.subject, &self.access, &visited, true)
			.visit(&mut serializer)?;
		serializer.end()
	}
}

struct PredicateObjects<'d, 'v, G: ?Sized + Graph, A> {
	graph: &'d G,
	subject: &'d G::Subject,
	predicate: &'d G::Predicate,
	access: &'d A,
	visited: &'v im::HashSet<&'d G::Subject>,
}

impl<'d, 'v, G: ?Sized + Graph, A: GraphAccess<G>, V: Vocabulary, I: Interpretation>
	LinkedDataPredicateObjects<V, I> for PredicateObjects<'d, 'v, G, A>
where
	G::Subject: Eq + Hash,
	G::Predicate: LinkedDataResource<V, I>,
	G::Object: LinkedDataResource<V, I>,
{
	fn visit_objects<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: PredicateObjectsVisitor<V, I>,
	{
		for object in self.graph.objects(self.subject, self.predicate) {
			visitor.object(&Object {
				graph: self.graph,
				object,
				access: self.access,
				visited: self.visited,
			})?;
		}

		visitor.end()
	}
}

impl<'a, 'v, G: ?Sized + Graph, A, V: Vocabulary, I: Interpretation> LinkedDataResource<V, I>
	for Object<'a, 'v, G, A>
where
	G::Object: LinkedDataResource<V, I>,
{
	fn interpretation(
		&self,
		vocabulary: &mut V,
		interpretation: &mut I,
	) -> ResourceInterpretation<V, I> {
		self.object.interpretation(vocabulary, interpretation)
	}
}

struct Object<'a, 'v, G: ?Sized + Graph, A> {
	graph: &'a G,
	object: &'a G::Object,
	access: &'a A,
	visited: &'v im::HashSet<&'a G::Subject>,
}

impl<'a, 'v, G: ?Sized + Graph, A: GraphAccess<G>, V: Vocabulary, I: Interpretation>
	LinkedDataSubject<V, I> for Object<'a, 'v, G, A>
where
	G::Subject: Eq + Hash,
	G::Predicate: LinkedDataResource<V, I>,
	G::Object: LinkedDataResource<V, I>,
{
	fn visit_subject<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<V, I>,
	{
		if let Some(subject) = self.access.object_as_subject(self.graph, self.object) {
			let mut visited = self.visited.clone();
			let visit_predicates = visited.insert(subject).is_none();

			Subject::new(self.graph, subject, self.access, &visited, visit_predicates)
				.visit(&mut visitor)?;
		}

		visitor.end()
	}
}

struct Subject<'a, 'v, G: ?Sized + Graph, A> {
	graph: &'a G,
	subject: &'a G::Subject,
	access: &'a A,
	visited: &'v im::HashSet<&'a G::Subject>,
	visit_predicates: bool,
}

impl<'a, 'v, G: ?Sized + Graph, A> Subject<'a, 'v, G, A>
where
	G::Subject: Eq + Hash,
{
	fn new(
		graph: &'a G,
		subject: &'a G::Subject,
		access: &'a A,
		visited: &'v im::HashSet<&'a G::Subject>,
		visit_predicates: bool,
	) -> Self {
		Self {
			graph,
			subject,
			access,
			visited,
			visit_predicates,
		}
	}

	fn visit<V: Vocabulary, I: Interpretation, S>(&self, visitor: &mut S) -> Result<(), S::Error>
	where
		A: GraphAccess<G>,
		S: SubjectVisitor<V, I>,
		G::Predicate: LinkedDataResource<V, I>,
		G::Object: LinkedDataResource<V, I>,
	{
		for (predicate, _) in self.graph.predicates(self.subject) {
			visitor.predicate(
				predicate,
				&PredicateObjects {
					graph: self.graph,
					subject: self.subject,
					predicate,
					access: self.access,
					visited: &self.visited,
				},
			)?;
		}

		Ok(())
	}
}

impl<'a, 'v, G: ?Sized + Graph, A: GraphAccess<G>, V: Vocabulary, I: Interpretation>
	LinkedDataSubject<V, I> for Subject<'a, 'v, G, A>
where
	G::Subject: Eq + Hash,
	G::Predicate: LinkedDataResource<V, I>,
	G::Object: LinkedDataResource<V, I>,
{
	fn visit_subject<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<V, I>,
	{
		if self.visit_predicates {
			for (predicate, _) in self.graph.predicates(self.subject) {
				visitor.predicate(
					predicate,
					&PredicateObjects {
						graph: self.graph,
						subject: self.subject,
						predicate,
						access: self.access,
						visited: &self.visited,
					},
				)?;
			}
		}

		visitor.end()
	}
}
