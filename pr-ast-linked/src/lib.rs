pub mod linked_statement;
pub mod object;

mod error;
mod dependency_resolver;
mod context_window;

mod parse_available_names;
mod parse_imports;
mod parse_types;
mod parse_declaration_types;
mod parser_linked_statement;

use linked_statement::GlobalLinkedStatement;
use object::{Object, ObjectFactory};
use pr_ast::statement::RStatement;
use pr_ast::SyntacticResult;
use pr_common::error::ErrorQueue;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Default)]
struct ModuleLiningContext {
    factory: ObjectFactory,

    files: Vec<FileLinkingContext>,
    file_paths: Vec<String>,
    type_statements: HashMap<Object, GlobalLinkedStatement>,
    type_statements_order: Vec<Object>, // consumed in parse_types
}

impl ModuleLiningContext {
    fn consume(self) -> LinkedModule {
        let factory = Arc::new(self.factory);

        let files = (self.files.into_iter()).zip(self.file_paths.into_iter()).map(|(m, path)| {
            let mut result = m.result;
            result.factory = factory.clone(); // FIXME: bad
            (result, path)
        }).collect();

        LinkedModule {
            factory,
            files,
            type_statements: self.type_statements,
            type_statements_order: self.type_statements_order,
        }
    }
}

#[derive(Default)]
struct FileLinkingContext {
    file_id: usize,

    available_names: HashMap<String, Object>,

    import_statements: Vec<RStatement>, // consumed in parse_imports
    type_statements: HashMap<Object, (RStatement, usize)>, // consumed in parse_types
    extern_statements: HashMap<Object, RStatement>, // consumed in parser_linked_statement
    function_statement: HashMap<Object, RStatement>, // consumed in parser_linked_statement
    variable_statement: HashMap<Object, RStatement>, // consumed in parser_linked_statement

    result: LinkedFile,
}

pub struct LinkedModule {
    pub factory: Arc<ObjectFactory>,

    pub type_statements: HashMap<Object, GlobalLinkedStatement>,
    pub type_statements_order: Vec<Object>, // consumed in parse_types
    pub files: Vec<(LinkedFile, String)>,
}

#[derive(Debug, Default)]
pub struct LinkedFile {
    pub factory: Arc<ObjectFactory>,

    pub extern_statements: HashMap<Object, GlobalLinkedStatement>,
    pub function_statement: HashMap<Object, GlobalLinkedStatement>,
    pub variable_statement: HashMap<Object, GlobalLinkedStatement>,
}

pub fn link_module(
    errors: &mut ErrorQueue,
    statements: Vec<(String, SyntacticResult)>,
) -> LinkedModule {
    let mut context = create_context(errors, statements);

    if errors.has_errors() { return context.consume() }

    parse_imports::parse_imports(errors, &mut context);

    parse_types::parse_types(errors, &mut context);
    parse_declaration_types::parse_declaration_types(errors, &mut context);

    if errors.has_errors() { return context.consume() }

    context.factory.asset_no_unknown();

    parser_linked_statement::link_objects(errors, &mut context);

    context.consume()
}

pub fn link_file(
    errors: &mut ErrorQueue,
    statements: SyntacticResult,
) -> LinkedModule {
    link_module(errors, vec![("unused string".to_string(), statements)])
}

fn create_context(
    errors: &mut ErrorQueue,
    statements: Vec<(String, SyntacticResult)>,
) -> ModuleLiningContext {
    let mut global_context = ModuleLiningContext {
        factory: ObjectFactory::default(),
        ..Default::default()
    };

    for (file_id, (path, result)) in statements.into_iter().enumerate() {
        let mut context = FileLinkingContext {
            file_id,
            ..Default::default()
        };

        parse_available_names::parse_available_names(errors, &mut context, &mut global_context.factory, result.statements);

        global_context.files.push(context);
        global_context.file_paths.push(path);
    }

    global_context
}
