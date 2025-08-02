pub mod linked_statement;

mod context_window;
mod parser_linked_statement;

use crate::error::CompilationError as CE;
use crate::parser::parse3_syntactic::statement::Statement;
use linked_statement::LinkedStatement;

pub fn link_variables<'text>(statement: &Vec<Statement<'text>>) -> Result<Vec<LinkedStatement<'text>>, CE> {
    parser_linked_statement::link_names(statement)
}
