pub mod statement;

mod parser_statements;
mod reorder_two_sided_ops;

use crate::error::CompilationError as CE;
use super::parse1_tokenize::token::TokenWithPos;
use statement::Statement;

pub fn parse_statements<'text>(tokens: &[TokenWithPos<'text>]) -> Result<Vec<Statement<'text>>, CE> {
    let statements = parser_statements::parse_statements(tokens)?;

    let reordered_statements = reorder_two_sided_ops::reorder_statements(statements);

    Ok(reordered_statements)
}
