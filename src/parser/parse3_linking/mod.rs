pub mod linked_statement;
pub mod object;

mod error;
mod dependency_resolver;
mod context_window;
mod parser_linked_statement;
mod parse_available_names;
mod parse_types;

use std::collections::HashMap;
use crate::Args;
use crate::error::CResult;
use crate::parser::parse2_syntactic::statement::Statement;
use linked_statement::{GlobalLinkedStatement};
use object::Object;
use crate::parser::parse3_linking::object::ObjectFactory;

#[derive(Default)]
struct TypeContext {
    factory: ObjectFactory,
    available_names: HashMap<String, Object>,

    // import_statements: HashMap<Object, Statement>,
    type_statements: HashMap<Object, Statement>,
    function_statement: HashMap<Object, Statement>,
    variable_statement: HashMap<Object, Statement>,

    result: LinkedProgram,
}

#[derive(Debug, Default)]
pub struct LinkedProgram {
    pub factory: ObjectFactory,

    pub type_statements: HashMap<Object, GlobalLinkedStatement>,
    pub function_statement: HashMap<Object, GlobalLinkedStatement>,
    pub variable_statement: HashMap<Object, GlobalLinkedStatement>,
}

pub fn link_all(args: &Args, statements: Vec<Statement>) -> CResult<LinkedProgram> {
    let mut context = TypeContext::default();

    parse_available_names::parse_available_names(&mut context, statements)?;
    parse_types::parse_types(&mut context)?;

    parser_linked_statement::link_objects(&mut context)
        .map_err(|err| { println!("{err}"); })?;

    context.result.factory = context.factory;
    Ok(context.result)
}
