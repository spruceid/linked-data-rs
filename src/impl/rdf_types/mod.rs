use std::{cell::RefCell, collections::HashSet, hash::Hash};

mod dataset_graph_view;
mod dataset_view;

struct ScopedPathEntry<'a, T: Copy + Eq + Hash> {
	path: &'a RefCell<HashSet<T>>,
	entry: Option<T>,
}

impl<'a, T: Copy + Eq + Hash> ScopedPathEntry<'a, T> {
	fn enter(path: &'a RefCell<HashSet<T>>, entry: T) -> Self {
		let inserted = path.borrow_mut().insert(entry);
		Self {
			path,
			entry: if inserted { Some(entry) } else { None },
		}
	}

	fn was_inserted(&self) -> bool {
		self.entry.is_some()
	}
}

impl<T: Copy + Eq + Hash> Drop for ScopedPathEntry<'_, T> {
	fn drop(&mut self) {
		if let Some(entry) = self.entry {
			let removed = self.path.borrow_mut().remove(&entry);
			debug_assert!(removed);
		}
	}
}
