pub mod linked_statement;
pub mod object;

mod context_window;
mod parser_linked_statement;

use crate::error::CompilationError as CE;
use crate::parser::parse2_syntactic::statement::Statement;
use linked_statement::LinkedStatement;
use object::ObjectFactory;

pub fn link_variables<'text>(statement: &Vec<Statement<'text>>, object_factory: &mut ObjectFactory) -> Result<Vec<LinkedStatement<'text>>, CE> {
    parser_linked_statement::link_names(statement, object_factory)
}
