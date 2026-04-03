pub mod linked_statement;
pub mod object;

mod error;
mod dependency_resolver;
mod context_window;

mod parse_imports;
mod parser_linked_statement;
mod parse_available_names;
mod parse_types;

use std::collections::HashMap;
use pr_common::error::ErrorQueue;
use pr_ast::statement::RStatement;
use linked_statement::GlobalLinkedStatement;
use object::{Object, ObjectFactory};
use pr_ast::SyntacticResult;

struct GlobalLiningContext {
    factory: ObjectFactory,

    modules: Vec<ModuleLinkingContext>,
    module_paths: Vec<String>,
}
impl GlobalLiningContext {
    fn consume(self) -> LinkedProgram {
        let factory = self.factory; // TODO: clone in parse_linked_statements

        let modules = self.modules.into_iter().map(|m| {
            let mut result = m.result;
            result.factory = factory.clone();
            result
        }).collect();

        LinkedProgram {
            factory,
            modules,
        }
    }
}

pub struct ModuleLinkingContext {
    module_id: usize,

    available_names: HashMap<String, Object>,

    // import_statements: HashMap<Object, RStatement>,
    type_statements: HashMap<Object, (RStatement, usize)>, // consumed in parse_types
    extern_statements: HashMap<Object, RStatement>, // consumed in parser_linked_statement
    function_statement: HashMap<Object, RStatement>, // consumed in parser_linked_statement
    variable_statement: HashMap<Object, RStatement>, // consumed in parser_linked_statement

    result: LinkedModule,
}

pub struct LinkedProgram {
    pub factory: ObjectFactory,

    pub modules: Vec<LinkedModule>,
}

#[derive(Debug, Default)]
pub struct LinkedModule {
    pub factory: ObjectFactory,

    // pub import_statements: Vec<Object>,
    pub type_statements_order: Vec<Object>,
    pub type_statements: HashMap<Object, GlobalLinkedStatement>,
    pub extern_statements: HashMap<Object, GlobalLinkedStatement>,
    pub function_statement: HashMap<Object, GlobalLinkedStatement>,
    pub variable_statement: HashMap<Object, GlobalLinkedStatement>,
}

pub fn link_all_modules(
    errors: &mut ErrorQueue,
    statements: Vec<(String, SyntacticResult)>,
) -> LinkedProgram {
    let mut context = create_context(errors, statements);

    if errors.has_errors() { return context.consume() }

    parse_imports::parse_imports(errors, &mut context);

    parse_types::parse_types(errors, &mut context);

    parser_linked_statement::link_objects(errors, &mut context);

    context.consume()
}

pub fn link_module(
    errors: &mut ErrorQueue,
    statements: SyntacticResult,
) -> LinkedModule {
    let mut program = link_all_modules(
        errors,
        vec![("unused string".to_string(), statements)]
    );
    program.modules.pop().unwrap()
}

fn create_context(
    errors: &mut ErrorQueue,
    statements: Vec<(String, SyntacticResult)>,
) -> GlobalLiningContext {
    let mut global_context = GlobalLiningContext {
        factory: ObjectFactory::default(),
        modules: vec![],
        module_paths: vec![],
    };

    for (module_id, (path, result)) in statements.into_iter().enumerate() {
        let mut context = ModuleLinkingContext {
            module_id,
            available_names: Default::default(),
            type_statements: Default::default(),
            extern_statements: Default::default(),
            function_statement: Default::default(),
            variable_statement: Default::default(),
            result: Default::default(),
        };

        parse_available_names::parse_available_names(errors, &mut context, &mut global_context.factory, result.statements);

        global_context.modules.push(context);
        global_context.module_paths.push(path);
    }

    global_context
}
