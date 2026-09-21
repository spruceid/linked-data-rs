use std::{
	cell::Cell,
	panic::{catch_unwind, AssertUnwindSafe},
	rc::Rc,
};

use iref::IriBuf;
use linked_data::{
	to_lexical_quads, to_lexical_subject_quads, GraphVisitor, LinkedData, LinkedDataGraph,
	LinkedDataPredicateObjects, LinkedDataResource, LinkedDataSubject, PredicateObjectsVisitor,
	RdfQuad, ResourceInterpretation, SubjectVisitor, Visitor,
};
use rdf_types::{
	dataset::{DatasetGraphView, DatasetView, IndexedBTreeDataset},
	generator, Id, Interpretation, Quad, Term, Vocabulary,
};

const EDGE: &str = "https://example.com/edge";

struct DefaultGraph<T>(T);

struct IdentifiedSubject<R, T>(R, T);

impl<I, V, T> LinkedData<I, V> for DefaultGraph<T>
where
	I: Interpretation,
	V: Vocabulary,
	T: LinkedDataGraph<I, V>,
{
	fn visit<S>(&self, mut visitor: S) -> Result<S::Ok, S::Error>
	where
		S: Visitor<I, V>,
	{
		visitor.default_graph(&self.0)?;
		visitor.end()
	}
}

impl<I, V, R, T> LinkedDataResource<I, V> for IdentifiedSubject<R, T>
where
	I: Interpretation,
	V: Vocabulary,
	R: LinkedDataResource<I, V>,
{
	fn interpretation(
		&self,
		vocabulary: &mut V,
		interpretation: &mut I,
	) -> ResourceInterpretation<'_, I, V> {
		self.0.interpretation(vocabulary, interpretation)
	}
}

impl<I, V, R, T> LinkedDataSubject<I, V> for IdentifiedSubject<R, T>
where
	I: Interpretation,
	V: Vocabulary,
	T: LinkedDataSubject<I, V>,
{
	fn visit_subject<S>(&self, visitor: S) -> Result<S::Ok, S::Error>
	where
		S: SubjectVisitor<I, V>,
	{
		self.1.visit_subject(visitor)
	}
}

fn iri(value: impl Into<String>) -> IriBuf {
	IriBuf::new(value.into()).expect("test IRIs are valid")
}

fn id(value: impl Into<String>) -> Id {
	Id::Iri(iri(value))
}

fn resource(value: impl Into<String>) -> Term {
	Term::Id(id(value))
}

fn dataset_quad(subject: &str, predicate: &str, object: &str, graph: Option<&str>) -> Quad {
	Quad(
		resource(subject),
		resource(predicate),
		resource(object),
		graph.map(resource),
	)
}

fn lexical_quad(subject: &str, predicate: &str, object: &str, graph: Option<&str>) -> RdfQuad {
	Quad(
		id(subject),
		iri(predicate),
		Term::Id(id(object)),
		graph.map(id),
	)
}

macro_rules! ignore_non_predicate_subject_events {
	() => {
		fn reverse_predicate<L, T>(
			&mut self,
			_predicate: &L,
			_subjects: &T,
		) -> Result<(), Self::Error>
		where
			L: ?Sized + LinkedDataResource<I, V>,
			T: ?Sized + LinkedDataPredicateObjects<I, V>,
		{
			Ok(())
		}

		fn graph<T>(&mut self, _value: &T) -> Result<(), Self::Error>
		where
			T: ?Sized + LinkedDataGraph<I, V>,
		{
			Ok(())
		}

		fn include<T>(&mut self, _value: &T) -> Result<(), Self::Error>
		where
			T: ?Sized + LinkedDataResource<I, V> + LinkedDataSubject<I, V>,
		{
			Ok(())
		}

		fn end(self) -> Result<Self::Ok, Self::Error> {
			Ok(())
		}
	};
}

enum PredicateProbe {
	Fail,
	Count(Rc<Cell<usize>>),
}

impl<I: Interpretation, V: Vocabulary> SubjectVisitor<I, V> for PredicateProbe {
	type Ok = ();
	type Error = &'static str;

	fn predicate<L, T>(&mut self, _predicate: &L, _objects: &T) -> Result<(), Self::Error>
	where
		L: ?Sized + LinkedDataResource<I, V>,
		T: ?Sized + LinkedDataPredicateObjects<I, V>,
	{
		match self {
			Self::Fail => Err("injected visitor failure"),
			Self::Count(count) => {
				count.set(count.get() + 1);
				Ok(())
			}
		}
	}

	ignore_non_predicate_subject_events!();
}

struct FailThenRetryObjects {
	failures: Rc<Cell<usize>>,
	retried_predicates: Rc<Cell<usize>>,
}

impl<I: Interpretation, V: Vocabulary> PredicateObjectsVisitor<I, V> for FailThenRetryObjects {
	type Ok = ();
	type Error = &'static str;

	fn object<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataResource<I, V> + LinkedDataSubject<I, V>,
	{
		let failure = value.visit_subject(PredicateProbe::Fail);
		assert_eq!(failure, Err("injected visitor failure"));
		self.failures.set(self.failures.get() + 1);

		value.visit_subject(PredicateProbe::Count(Rc::clone(&self.retried_predicates)))
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(())
	}
}

struct ProbeNestedObjectCleanup {
	failures: Rc<Cell<usize>>,
	retried_predicates: Rc<Cell<usize>>,
}

impl<I: Interpretation, V: Vocabulary> SubjectVisitor<I, V> for ProbeNestedObjectCleanup {
	type Ok = ();
	type Error = &'static str;

	fn predicate<L, T>(&mut self, _predicate: &L, objects: &T) -> Result<(), Self::Error>
	where
		L: ?Sized + LinkedDataResource<I, V>,
		T: ?Sized + LinkedDataPredicateObjects<I, V>,
	{
		objects.visit_objects(FailThenRetryObjects {
			failures: Rc::clone(&self.failures),
			retried_predicates: Rc::clone(&self.retried_predicates),
		})
	}

	ignore_non_predicate_subject_events!();
}

fn probe_nested_object_cleanup<T>(
	value: &T,
	visitor: ProbeNestedObjectCleanup,
) -> Result<(), &'static str>
where
	T: LinkedDataSubject<rdf_types::interpretation::WithGenerator<generator::Blank>, ()>,
{
	value.visit_subject(visitor)
}

#[derive(Clone)]
enum NamedGraphProbe {
	Fail,
	Panic,
	Count(Rc<Cell<usize>>),
}

impl<I: Interpretation, V: Vocabulary> SubjectVisitor<I, V> for NamedGraphProbe {
	type Ok = ();
	type Error = &'static str;

	fn predicate<L, T>(&mut self, _predicate: &L, objects: &T) -> Result<(), Self::Error>
	where
		L: ?Sized + LinkedDataResource<I, V>,
		T: ?Sized + LinkedDataPredicateObjects<I, V>,
	{
		objects.visit_objects(NamedGraphObjects {
			probe: self.clone(),
		})
	}

	fn reverse_predicate<L, T>(&mut self, _predicate: &L, _subjects: &T) -> Result<(), Self::Error>
	where
		L: ?Sized + LinkedDataResource<I, V>,
		T: ?Sized + LinkedDataPredicateObjects<I, V>,
	{
		Ok(())
	}

	fn graph<T>(&mut self, _value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataGraph<I, V>,
	{
		match self {
			Self::Fail => Err("injected named-graph visitor failure"),
			Self::Panic => panic!("injected named-graph visitor panic"),
			Self::Count(count) => {
				count.set(count.get() + 1);
				Ok(())
			}
		}
	}

	fn include<T>(&mut self, _value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataResource<I, V> + LinkedDataSubject<I, V>,
	{
		Ok(())
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(())
	}
}

struct NamedGraphObjects {
	probe: NamedGraphProbe,
}

impl<I: Interpretation, V: Vocabulary> PredicateObjectsVisitor<I, V> for NamedGraphObjects {
	type Ok = ();
	type Error = &'static str;

	fn object<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataResource<I, V> + LinkedDataSubject<I, V>,
	{
		value.visit_subject(self.probe.clone())
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(())
	}
}

#[derive(Clone, Copy)]
enum ProbeFailure {
	Error,
	Panic,
}

struct ProbeNamedGraphCleanup {
	failure: ProbeFailure,
	failures: Rc<Cell<usize>>,
	retried_graphs: Rc<Cell<usize>>,
}

impl<I: Interpretation, V: Vocabulary> GraphVisitor<I, V> for ProbeNamedGraphCleanup {
	type Ok = ();
	type Error = &'static str;

	fn subject<T>(&mut self, value: &T) -> Result<(), Self::Error>
	where
		T: ?Sized + LinkedDataResource<I, V> + LinkedDataSubject<I, V>,
	{
		let failed = match self.failure {
			ProbeFailure::Error => match value.visit_subject(NamedGraphProbe::Fail) {
				Err("injected named-graph visitor failure") => true,
				Err(error) => return Err(error),
				Ok(()) => false,
			},
			ProbeFailure::Panic => catch_unwind(AssertUnwindSafe(|| {
				value.visit_subject(NamedGraphProbe::Panic)
			}))
			.is_err(),
		};

		if failed {
			self.failures.set(self.failures.get() + 1);
			value.visit_subject(NamedGraphProbe::Count(Rc::clone(&self.retried_graphs)))?;
		}

		Ok(())
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		Ok(())
	}
}

fn probe_named_graph_cleanup<T>(
	value: &T,
	visitor: ProbeNamedGraphCleanup,
) -> Result<(), &'static str>
where
	T: LinkedDataGraph<rdf_types::interpretation::WithGenerator<generator::Blank>, ()>,
{
	value.visit_graph(visitor)
}

fn assert_dataset_view_named_graph_cleanup(failure: ProbeFailure) {
	const ROOT: &str = "https://example.com/root";
	const GRAPH: &str = "https://example.com/graph";
	const NODE: &str = "https://example.com/node";
	const LEAF: &str = "https://example.com/leaf";

	let mut dataset = IndexedBTreeDataset::new();
	dataset.insert(dataset_quad(ROOT, EDGE, GRAPH, None));
	dataset.insert(dataset_quad(NODE, EDGE, LEAF, Some(GRAPH)));

	let failures = Rc::new(Cell::new(0));
	let retried_graphs = Rc::new(Cell::new(0));
	let view = DatasetView {
		dataset: &dataset,
		graph: None,
	};

	let result = probe_named_graph_cleanup(
		&view,
		ProbeNamedGraphCleanup {
			failure,
			failures: Rc::clone(&failures),
			retried_graphs: Rc::clone(&retried_graphs),
		},
	);
	result.expect("a handled visitor failure must not poison the active graph path");

	assert_eq!(failures.get(), 1, "the injected failure must be observed");
	assert_eq!(
		retried_graphs.get(),
		1,
		"the named graph must be revisited after path cleanup",
	);
}

#[test]
fn dataset_view_terminates_named_graph_cycle_in_stable_order() {
	const ROOT: &str = "https://example.com/root";
	const GRAPH_A: &str = "https://example.com/graph-a";
	const GRAPH_B: &str = "https://example.com/graph-b";
	const NODE_A: &str = "https://example.com/node-a";
	const NODE_B: &str = "https://example.com/node-b";

	let mut dataset = IndexedBTreeDataset::new();
	dataset.insert(dataset_quad(ROOT, EDGE, GRAPH_A, None));
	dataset.insert(dataset_quad(NODE_A, EDGE, GRAPH_B, Some(GRAPH_A)));
	dataset.insert(dataset_quad(NODE_B, EDGE, GRAPH_A, Some(GRAPH_B)));

	let view = DatasetView {
		dataset: &dataset,
		graph: None,
	};
	let quads = to_lexical_quads(generator::Blank::new(), &DefaultGraph(view))
		.expect("cyclic named graphs must terminate");

	assert_eq!(
		quads,
		vec![
			lexical_quad(NODE_B, EDGE, GRAPH_A, Some(GRAPH_B)),
			lexical_quad(NODE_A, EDGE, GRAPH_B, Some(GRAPH_A)),
			lexical_quad(ROOT, EDGE, GRAPH_A, None),
		]
	);
}

#[test]
fn dataset_view_revisits_shared_named_graph_per_path_in_stable_order() {
	const ROOT: &str = "https://example.com/root";
	const GRAPH_LEFT: &str = "https://example.com/graph-left";
	const GRAPH_RIGHT: &str = "https://example.com/graph-right";
	const GRAPH_SHARED: &str = "https://example.com/graph-shared";
	const NODE_LEFT: &str = "https://example.com/node-left";
	const NODE_RIGHT: &str = "https://example.com/node-right";
	const NODE_SHARED: &str = "https://example.com/node-shared";
	const LEAF: &str = "https://example.com/leaf";
	const LEFT_EDGE: &str = "https://example.com/edge-a-left";
	const RIGHT_EDGE: &str = "https://example.com/edge-b-right";

	let mut dataset = IndexedBTreeDataset::new();
	dataset.insert(dataset_quad(ROOT, LEFT_EDGE, GRAPH_LEFT, None));
	dataset.insert(dataset_quad(ROOT, RIGHT_EDGE, GRAPH_RIGHT, None));
	dataset.insert(dataset_quad(
		NODE_LEFT,
		EDGE,
		GRAPH_SHARED,
		Some(GRAPH_LEFT),
	));
	dataset.insert(dataset_quad(
		NODE_RIGHT,
		EDGE,
		GRAPH_SHARED,
		Some(GRAPH_RIGHT),
	));
	dataset.insert(dataset_quad(NODE_SHARED, EDGE, LEAF, Some(GRAPH_SHARED)));

	let view = DatasetView {
		dataset: &dataset,
		graph: None,
	};
	let quads = to_lexical_quads(generator::Blank::new(), &DefaultGraph(view))
		.expect("a shared named graph must be revisited once per acyclic path");

	assert_eq!(
		quads,
		vec![
			lexical_quad(NODE_SHARED, EDGE, LEAF, Some(GRAPH_SHARED)),
			lexical_quad(NODE_LEFT, EDGE, GRAPH_SHARED, Some(GRAPH_LEFT)),
			lexical_quad(ROOT, LEFT_EDGE, GRAPH_LEFT, None),
			lexical_quad(NODE_SHARED, EDGE, LEAF, Some(GRAPH_SHARED)),
			lexical_quad(NODE_RIGHT, EDGE, GRAPH_SHARED, Some(GRAPH_RIGHT)),
			lexical_quad(ROOT, RIGHT_EDGE, GRAPH_RIGHT, None),
		]
	);
}

#[test]
fn dataset_view_terminates_mixed_subject_and_named_graph_cycles() {
	const ROOT: &str = "https://example.com/root";
	const GRAPH: &str = "https://example.com/graph";
	const NODE_A: &str = "https://example.com/node-a";
	const NODE_B: &str = "https://example.com/node-b";
	const BACK_EDGE: &str = "https://example.com/edge-b-back";
	const GRAPH_EDGE: &str = "https://example.com/edge-c-graph";

	let mut dataset = IndexedBTreeDataset::new();
	dataset.insert(dataset_quad(ROOT, EDGE, GRAPH, None));
	dataset.insert(dataset_quad(NODE_A, EDGE, NODE_B, Some(GRAPH)));
	dataset.insert(dataset_quad(NODE_B, BACK_EDGE, NODE_A, Some(GRAPH)));
	dataset.insert(dataset_quad(NODE_B, GRAPH_EDGE, GRAPH, Some(GRAPH)));

	let view = DatasetView {
		dataset: &dataset,
		graph: None,
	};
	let quads = to_lexical_quads(generator::Blank::new(), &DefaultGraph(view))
		.expect("combined subject and named-graph cycles must terminate");

	assert_eq!(
		quads,
		vec![
			lexical_quad(NODE_A, EDGE, NODE_B, Some(GRAPH)),
			lexical_quad(NODE_B, BACK_EDGE, NODE_A, Some(GRAPH)),
			lexical_quad(NODE_B, GRAPH_EDGE, GRAPH, Some(GRAPH)),
			lexical_quad(ROOT, EDGE, GRAPH, None),
		]
	);
}

#[test]
fn dataset_view_restores_named_graph_paths_after_a_visitor_error() {
	assert_dataset_view_named_graph_cleanup(ProbeFailure::Error);
}

#[test]
fn dataset_view_restores_named_graph_paths_after_a_visitor_panic() {
	assert_dataset_view_named_graph_cleanup(ProbeFailure::Panic);
}

#[test]
fn dataset_graph_view_terminates_self_cycle_in_stable_order() {
	const ROOT: &str = "https://example.com/root";
	const SEED: &str = "https://example.com/seed";

	let mut dataset = IndexedBTreeDataset::new();
	// Seed `ROOT` so the indexed dataset reuses one resource slot when it is
	// both the subject and object of the following quad.
	dataset.insert(dataset_quad(SEED, EDGE, ROOT, None));
	dataset.insert(dataset_quad(ROOT, EDGE, ROOT, None));

	let root = resource(ROOT);
	let view = DatasetGraphView {
		dataset: &dataset,
		graph: None,
		resource: &root,
	};
	let subject_view = IdentifiedSubject(&root, view);
	let (subject, quads) = to_lexical_subject_quads(generator::Blank::new(), None, &subject_view)
		.expect("a subject self-cycle must terminate");

	assert_eq!(subject, id(ROOT));
	assert_eq!(quads, vec![lexical_quad(ROOT, EDGE, ROOT, None)]);
}

#[test]
fn dataset_graph_view_terminates_two_subject_cycle_in_stable_postorder() {
	const NODE_A: &str = "https://example.com/node-a";
	const NODE_B: &str = "https://example.com/node-b";

	let mut dataset = IndexedBTreeDataset::new();
	dataset.insert(dataset_quad(NODE_A, EDGE, NODE_B, None));
	dataset.insert(dataset_quad(NODE_B, EDGE, NODE_A, None));

	let root = resource(NODE_A);
	let view = DatasetGraphView {
		dataset: &dataset,
		graph: None,
		resource: &root,
	};
	let subject_view = IdentifiedSubject(&root, view);
	let (subject, quads) = to_lexical_subject_quads(generator::Blank::new(), None, &subject_view)
		.expect("a two-subject cycle must terminate");

	assert_eq!(subject, id(NODE_A));
	assert_eq!(
		quads,
		vec![
			lexical_quad(NODE_B, EDGE, NODE_A, None),
			lexical_quad(NODE_A, EDGE, NODE_B, None),
		]
	);
}

#[test]
fn dataset_graph_view_revisits_shared_subject_per_path_in_stable_order() {
	const ROOT: &str = "https://example.com/root";
	const LEFT: &str = "https://example.com/left";
	const RIGHT: &str = "https://example.com/right";
	const SHARED: &str = "https://example.com/shared";
	const LEAF: &str = "https://example.com/leaf";
	const LEFT_EDGE: &str = "https://example.com/edge-a-left";
	const RIGHT_EDGE: &str = "https://example.com/edge-b-right";

	let mut dataset = IndexedBTreeDataset::new();
	dataset.insert(dataset_quad(ROOT, LEFT_EDGE, LEFT, None));
	dataset.insert(dataset_quad(ROOT, RIGHT_EDGE, RIGHT, None));
	dataset.insert(dataset_quad(LEFT, EDGE, SHARED, None));
	dataset.insert(dataset_quad(RIGHT, EDGE, SHARED, None));
	dataset.insert(dataset_quad(SHARED, EDGE, LEAF, None));

	let root = resource(ROOT);
	let view = DatasetGraphView {
		dataset: &dataset,
		graph: None,
		resource: &root,
	};
	let subject_view = IdentifiedSubject(&root, view);
	let (subject, quads) = to_lexical_subject_quads(generator::Blank::new(), None, &subject_view)
		.expect("repeated subjects must terminate");

	assert_eq!(subject, id(ROOT));
	assert_eq!(
		quads,
		vec![
			lexical_quad(SHARED, EDGE, LEAF, None),
			lexical_quad(LEFT, EDGE, SHARED, None),
			lexical_quad(ROOT, LEFT_EDGE, LEFT, None),
			lexical_quad(SHARED, EDGE, LEAF, None),
			lexical_quad(RIGHT, EDGE, SHARED, None),
			lexical_quad(ROOT, RIGHT_EDGE, RIGHT, None),
		]
	);
}

#[test]
fn dataset_graph_view_restores_the_path_after_a_visitor_error() {
	const ROOT: &str = "https://example.com/root";
	const CHILD: &str = "https://example.com/child";
	const LEAF: &str = "https://example.com/leaf";

	let mut dataset = IndexedBTreeDataset::new();
	dataset.insert(dataset_quad(ROOT, EDGE, CHILD, None));
	dataset.insert(dataset_quad(CHILD, EDGE, LEAF, None));

	let root = resource(ROOT);
	let subject_view = IdentifiedSubject(
		&root,
		DatasetGraphView {
			dataset: &dataset,
			graph: None,
			resource: &root,
		},
	);
	let failures = Rc::new(Cell::new(0));
	let retried_predicates = Rc::new(Cell::new(0));

	probe_nested_object_cleanup(
		&subject_view,
		ProbeNestedObjectCleanup {
			failures: Rc::clone(&failures),
			retried_predicates: Rc::clone(&retried_predicates),
		},
	)
	.expect("a handled visitor error must not poison the active traversal path");

	assert_eq!(
		failures.get(),
		1,
		"the injected nested error must propagate"
	);
	assert_eq!(
		retried_predicates.get(),
		1,
		"the child predicates must be revisited after path cleanup",
	);
}

#[test]
fn dataset_graph_view_preserves_deep_postorder_traversal() {
	const DEPTH: usize = 2_048;
	const STACK_SIZE: usize = 16 * 1024 * 1024;

	std::thread::Builder::new()
		.name("deep-dataset-graph-view".into())
		.stack_size(STACK_SIZE)
		.spawn(|| {
			fn node(index: usize) -> String {
				format!("https://example.com/node/{index:04}")
			}

			let mut dataset = IndexedBTreeDataset::new();
			for index in 0..DEPTH {
				dataset.insert(dataset_quad(&node(index), EDGE, &node(index + 1), None));
			}

			let root = resource(node(0));
			let view = DatasetGraphView {
				dataset: &dataset,
				graph: None,
				resource: &root,
			};
			let subject_view = IdentifiedSubject(&root, view);
			let (_, quads) = to_lexical_subject_quads(generator::Blank::new(), None, &subject_view)
				.expect("deep acyclic traversal must succeed");
			let expected: Vec<_> = (0..DEPTH)
				.rev()
				.map(|index| lexical_quad(&node(index), EDGE, &node(index + 1), None))
				.collect();

			assert_eq!(quads, expected);
		})
		.expect("deep traversal test thread must start")
		.join()
		.expect("deep traversal test thread must not panic");
}

#[test]
#[ignore = "manual bounded performance comparison"]
fn benchmark_deep_and_cyclic_traversal() {
	const DEPTH: usize = 256;
	const DEEP_ITERATIONS: u32 = 100;
	const CYCLIC_ITERATIONS: u32 = 1_000;

	fn node(index: usize) -> String {
		format!("https://example.com/bench/node/{index:04}")
	}

	let mut deep_dataset = IndexedBTreeDataset::new();
	for index in 0..DEPTH {
		deep_dataset.insert(dataset_quad(&node(index), EDGE, &node(index + 1), None));
	}
	let root = resource(node(0));
	let deep_view = IdentifiedSubject(
		&root,
		DatasetGraphView {
			dataset: &deep_dataset,
			graph: None,
			resource: &root,
		},
	);
	let started = std::time::Instant::now();
	for _ in 0..DEEP_ITERATIONS {
		let (_, quads) = to_lexical_subject_quads(generator::Blank::new(), None, &deep_view)
			.expect("deep benchmark traversal must succeed");
		assert_eq!(quads.len(), DEPTH);
		std::hint::black_box(quads);
	}
	let deep_elapsed = started.elapsed();

	const ROOT: &str = "https://example.com/bench/root";
	const GRAPH_A: &str = "https://example.com/bench/graph-a";
	const GRAPH_B: &str = "https://example.com/bench/graph-b";
	let mut cyclic_dataset = IndexedBTreeDataset::new();
	cyclic_dataset.insert(dataset_quad(ROOT, EDGE, GRAPH_A, None));
	cyclic_dataset.insert(dataset_quad(
		"https://example.com/bench/node-a",
		EDGE,
		GRAPH_B,
		Some(GRAPH_A),
	));
	cyclic_dataset.insert(dataset_quad(
		"https://example.com/bench/node-b",
		EDGE,
		GRAPH_A,
		Some(GRAPH_B),
	));
	let cyclic_view = DefaultGraph(DatasetView {
		dataset: &cyclic_dataset,
		graph: None,
	});
	let started = std::time::Instant::now();
	for _ in 0..CYCLIC_ITERATIONS {
		let quads = to_lexical_quads(generator::Blank::new(), &cyclic_view)
			.expect("cyclic benchmark traversal must succeed");
		assert_eq!(quads.len(), 3);
		std::hint::black_box(quads);
	}
	let cyclic_elapsed = started.elapsed();

	println!(
		"deep: depth={DEPTH}, iterations={DEEP_ITERATIONS}, total={deep_elapsed:?}, ns/iteration={}; cyclic: iterations={CYCLIC_ITERATIONS}, total={cyclic_elapsed:?}, ns/iteration={}",
		deep_elapsed.as_nanos() / u128::from(DEEP_ITERATIONS),
		cyclic_elapsed.as_nanos() / u128::from(CYCLIC_ITERATIONS),
	);
}
