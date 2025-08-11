use std::collections::HashMap;
use super::object::Object;
use crate::error::CompilationError as CE;

#[derive(Debug, Default)]
struct ObjectsContext(HashMap<String, Object>);

impl ObjectsContext {
    fn add(&mut self, name: String, object: Object) {
        self.0.insert(name, object);
    }
    fn get(&self, name: &String) -> Option<Object> {
        self.0.get(name).copied()
    }
}

#[derive(Debug)]
pub struct ObjectContextWindow {
    contexts: Vec<ObjectsContext>,
}

impl ObjectContextWindow {
    pub fn new() -> Self {
        ObjectContextWindow { contexts: vec![ObjectsContext::default()] }
    }
    pub fn step_in(&mut self) {
        self.contexts.push(ObjectsContext::default());
    }
    pub fn step_out(&mut self) {
        assert!(!self.contexts.is_empty(), "No more objects to step out!");
        self.contexts.pop();
    }
    pub fn get(&self, name: &String) -> Option<Object> {
        self.contexts.iter().rev()
            .find_map(|obj_con|obj_con.get(name))
    }
    pub fn get_or_error(&self, name: &String) -> Result<Object, CE> {
        match self.get(name) {
            Some(obj) => Ok(obj),
            None => Err(CE::LinkingError { name: name.clone(), context: format!("{self:?}") })
        }
    }
    pub fn add(&mut self, name: String, object: Object) {
        self.contexts.last_mut().unwrap().add(name, object);
    }
}
