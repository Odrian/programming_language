pub mod linked_statement;
pub mod object;

mod context_window;
mod parser_linked_statement;

use crate::error::CResult;
use crate::parser::parse2_syntactic::statement::Statement;
use linked_statement::GlobalLinkedStatement;
use object::ObjectFactory;

pub fn link_variables(statement: Vec<Statement>, object_factory: &mut ObjectFactory) -> CResult<Vec<GlobalLinkedStatement>> {
    parser_linked_statement::link_names(statement, object_factory)
        .map_err(|err| { println!("{err}"); })
}
