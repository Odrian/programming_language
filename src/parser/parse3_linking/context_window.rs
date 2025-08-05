use std::collections::HashMap;

use super::linked_statement::{Object, ObjType};

#[derive(Debug, Default)]
struct ObjectsContext<'text>(HashMap<String, Object<'text>>);

impl<'text> ObjectsContext<'text> {
    fn add(&mut self, name_string: String, name: &'text [char], name_id: u32, v_type: ObjType) -> Object<'text> {
        let object = Object::new(name, name_id, v_type);
        self.0.insert(name_string, object);
        object
    }
    fn get(&self, name: &String) -> Option<&Object<'text>> {
        self.0.get(name)
    }
}

#[derive(Debug)]
pub struct ObjectContextWindow<'text> {
    contexts: Vec<ObjectsContext<'text>>,
}

impl<'text> ObjectContextWindow<'text> {
    pub fn new() -> Self {
        ObjectContextWindow { contexts: vec![] }
    }
    pub fn step_in(&mut self) {
        self.contexts.push(ObjectsContext::default());
    }
    pub fn step_out(&mut self) {
        assert!(!self.contexts.is_empty(), "No more objects to step out!");
        self.contexts.pop();
    }
    pub fn get(&self, name: &String) -> Option<&Object<'text>> {
        self.contexts.iter().rev()
            .find_map(|obj_con|obj_con.get(name))
    }
    pub fn add(&mut self, name: &'text [char], v_type: ObjType) -> Object<'text> {
        let name_string = name.iter().collect::<String>();
        let name_id = self.contexts.iter().rev().find_map(|context|
            context.0.get(&name_string).map(|obj| obj.name_id + 1)
        ).unwrap_or(0);

        self.contexts.last_mut().unwrap().add(name_string, name, name_id, v_type)
    }
}
