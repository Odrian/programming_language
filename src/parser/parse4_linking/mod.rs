use crate::error::CompilationError as CE;

pub mod linked_statement;

mod context_window;
mod parser_linked_statement;

use crate::parser::parse3_syntactic::statement::Statement;
use linked_statement::LinkedStatement;

pub fn link_variables(statement: Vec<Statement>) -> Result<Vec<LinkedStatement>, CE> {
    parser_linked_statement::link_variables(statement)
}
