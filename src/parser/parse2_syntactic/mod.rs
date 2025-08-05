pub mod statement;

mod parser_statements;

use crate::error::CompilationError as CE;
use super::parse1_tokenize::token::TokenWithPos;
use statement::Statement;

pub fn parse_statements<'text>(tokens: &[TokenWithPos<'text>]) -> Result<Vec<Statement<'text>>, CE> {
    let mut statements = Vec::new();

    let mut state = parser_statements::ParsingState::new(tokens);
    while !state.at_end() {
        statements.push(state.parse_statement()?);
    }

    Ok(statements)
}
