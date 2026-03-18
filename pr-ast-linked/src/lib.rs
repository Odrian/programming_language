pub mod linked_statement;
pub mod object;

mod error;
mod dependency_resolver;
mod context_window;
mod parser_linked_statement;
mod parse_available_names;
mod parse_types;

use std::collections::HashMap;
use pr_common::error::ErrorQueue;
use pr_ast::statement::RStatement;
use linked_statement::GlobalLinkedStatement;
use object::{Object, ObjectFactory};
use pr_ast::SyntacticResult;

#[derive(Default)]
struct TypeContext {
    factory: ObjectFactory,
    available_names: HashMap<String, Object>,

    // import_statements: HashMap<Object, RStatement>,
    type_statements: HashMap<Object, RStatement>,
    extern_statements: HashMap<Object, RStatement>,
    function_statement: HashMap<Object, RStatement>,
    variable_statement: HashMap<Object, RStatement>,

    result: LinkedProgram,
}
impl TypeContext {
    fn consume(mut self) -> LinkedProgram {
        self.result.factory = self.factory;
        self.result
    }
}

#[derive(Debug, Default)]
pub struct LinkedProgram {
    pub factory: ObjectFactory,

    pub type_statements_order: Vec<Object>,
    pub type_statements: HashMap<Object, GlobalLinkedStatement>,
    pub extern_statements: HashMap<Object, GlobalLinkedStatement>,
    pub function_statement: HashMap<Object, GlobalLinkedStatement>,
    pub variable_statement: HashMap<Object, GlobalLinkedStatement>,
}

pub fn link_ast(errors: &mut ErrorQueue, statements: SyntacticResult) -> LinkedProgram {
    let mut context = TypeContext::default();

    parse_available_names::parse_available_names(errors, &mut context, statements.statements);

    if errors.has_errors() { return context.consume() }

    parse_types::parse_types(errors, &mut context);

    parser_linked_statement::link_objects(errors, &mut context);

    context.consume()
}
