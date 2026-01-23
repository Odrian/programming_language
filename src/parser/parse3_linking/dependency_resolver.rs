use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;
use crate::error::CResult;
use crate::parser::parse3_linking::error::LinkingError;

pub struct DependencyResolver<Key: Eq + Hash + Copy> {
    queue: VecDeque<Key>,
    waiting_for: HashMap<Key, HashSet<Key>>, // {a, {b..}} - a need b to compile
    waiting_rev: HashMap<Key, HashSet<Key>>,     // {b, {a..}} - a need b to compile
}

impl<Key: Eq + Hash + Copy> Default for DependencyResolver<Key> {
    fn default() -> Self {
        Self { queue: Default::default(), waiting_for: Default::default(), waiting_rev: Default::default() }
    }
}

impl<Key: Eq + Hash + Copy> DependencyResolver<Key> {
    pub fn is_empty(&self) -> bool {
        self.queue.is_empty() && self.waiting_for.is_empty() && self.waiting_rev.is_empty()
    }
    pub fn add(&mut self, keys: impl IntoIterator<Item=Key>) {
        self.queue.extend(keys);
    }
    pub fn clear(&mut self) {
        self.queue.clear();
        self.waiting_rev.clear();
        self.waiting_for.clear();
    }
    pub fn next(&mut self) -> CResult<Option<Key>> {
        let result = self.queue.pop_front();
        if result.is_none() && !self.waiting_for.is_empty() {
            self.get_dependency_cycle_error().print();
            return Err(())
        }
        Ok(result)
    }
    pub fn key_done(&mut self, depends_on: Key) {
        if let Some(whats) = self.waiting_rev.remove(&depends_on) {
            for what in whats {
                let what_map = self.waiting_for.entry(what).or_default();
                what_map.remove(&depends_on);
                if what_map.is_empty() {
                    self.waiting_for.remove(&what);
                    self.queue.push_back(what);
                }
            }
        }
    }
    fn get_dependency_cycle_error(&mut self) -> LinkingError {
        LinkingError::DependencyCycle
    }
    pub fn add_dependency(&mut self, what: Key, depends_on: Vec<Key>) {
        for &depends_on_one in &depends_on {
            self.waiting_rev.entry(depends_on_one).or_default().insert(what);
        }
        self.waiting_for.entry(what).or_default().extend(depends_on);
    }
}
