use std::collections::HashMap;

use super::linked_statement::{Object, ObjType};

#[derive(Debug, Default)]
struct ObjectsContext<'x>(HashMap<String, Object<'x>>);

impl<'x> ObjectsContext<'x> {
    fn add(&mut self, name_string: String, name: &'x [char], name_id: u32, v_type: ObjType) -> Object<'x> {
        let object = Object::new(name, name_id, v_type);
        self.0.insert(name_string, object);
        object
    }
    fn get(&self, name: &String) -> Option<&Object<'x>> {
        self.0.get(name)
    }
}

#[derive(Debug)]
pub struct ObjectContextWindow<'x> {
    contexts: Vec<ObjectsContext<'x>>,
}

impl<'x> ObjectContextWindow<'x> {
    pub fn new() -> Self {
        ObjectContextWindow { contexts: vec![] }
    }
    pub fn step_in(&mut self) {
        self.contexts.push(ObjectsContext::default());
    }
    pub fn step_out(&mut self) {
        if self.contexts.is_empty() {
            panic!("No more objects to step out!");
        }
        self.contexts.pop();
    }
    pub fn get(&self, name: &String) -> Option<&Object<'x>> {
        for object_context in self.contexts.iter().rev() {
            let object = object_context.get(name);
            if object.is_some() {
                return object;
            }
        }
        None
    }
    pub fn add(&mut self, name: &'x [char], v_type: ObjType) -> Object<'x> {
        let name_string = name.iter().collect::<String>();
        let name_id = self.contexts.iter().rev().find_map(|context|
            context.0.get(&name_string).map(|obj| obj.name_id + 1)
        ).unwrap_or(0);

        self.contexts.last_mut().unwrap().add(name_string, name, name_id, v_type)
    }
}
