use std::collections::HashMap;
use super::object::Object;
use crate::error::CompilationError as CE;

#[derive(Debug, Default)]
struct ObjectsContext<'text>(HashMap<String, Object<'text>>);

impl<'text> ObjectsContext<'text> {
    fn add(&mut self, object: Object<'text>) {
        let name_string = object.name.iter().collect::<String>();
        self.0.insert(name_string, object);
    }
    fn get(&self, name: &String) -> Option<Object<'text>> {
        self.0.get(name).copied()
    }
}

#[derive(Debug)]
pub struct ObjectContextWindow<'text> {
    contexts: Vec<ObjectsContext<'text>>,
}

impl<'text> ObjectContextWindow<'text> {
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
    pub fn get(&self, name: &[char]) -> Option<Object<'text>> {
        let name = name.iter().collect::<String>();
        self.contexts.iter().rev()
            .find_map(|obj_con|obj_con.get(&name))
    }
    pub fn get_or_error(&self, name: &[char]) -> Result<Object<'text>, CE> {
        match self.get(name) {
            Some(obj) => Ok(obj),
            None => Err(CE::LinkingError { name: name.iter().collect::<String>(), context: format!("{self:?}") })
        }
    }
    pub fn add(&mut self, object: Object<'text>) {
        self.contexts.last_mut().unwrap().add(object);
    }
}
