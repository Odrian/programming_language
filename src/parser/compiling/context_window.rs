use std::collections::HashMap;
use inkwell::values::{AnyValueEnum, FunctionValue, PointerValue};
use crate::parser::parse3_linking::object::Object;

#[derive(Debug, Default)]
struct ValueContext<'ctx>(HashMap<Object, AnyValueEnum<'ctx>>);

impl<'ctx> ValueContext<'ctx> {
    fn add(&mut self, object: Object, value: AnyValueEnum<'ctx>) {
        self.0.insert(object, value);
    }
    fn get(&self, object: Object) -> Option<&AnyValueEnum<'ctx>> {
        self.0.get(&object)
    }
}

#[derive(Debug, Default)]
pub struct ValueContextWindow<'ctx> {
    contexts: Vec<ValueContext<'ctx>>,
}

impl<'ctx> ValueContextWindow<'ctx> {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn step_in(&mut self) {
        self.contexts.push(ValueContext::default());
    }
    pub fn step_out(&mut self) {
        assert!(!self.contexts.is_empty(), "No more objects to step out!");
        self.contexts.pop();
    }
    fn get(&self, object: Object) -> Option<&AnyValueEnum<'ctx>> {
        self.contexts.iter().rev()
            .find_map(|obj_con|obj_con.get(object))
    }
    pub fn get_pointer_unwrap(&self, object: Object) -> PointerValue<'ctx> {
        self.get(object).unwrap().into_pointer_value()
    }
    pub fn get_function_unwrap(&self, object: Object) -> FunctionValue<'ctx> {
        self.get(object).unwrap().into_function_value()
    }
    pub fn add(&mut self, object: Object, value: AnyValueEnum<'ctx>) {
        self.contexts.last_mut().unwrap().add(object, value);
    }
}
