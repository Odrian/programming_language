use crate::error::CompilationError as CE;

pub mod statement;

mod parser_statements;

use super::parse1_tokenize::token::TokenWithPos;
use statement::Statement;

pub fn parse_statements<'x>(tokens: &[TokenWithPos<'x>]) -> Result<Vec<Statement<'x>>, CE> {
    let mut statements = Vec::new();

    let mut state = parser_statements::ParsingState::new(tokens);
    while !state.at_end() {
        statements.push(state.parse_statement()?);
    }

    Ok(statements)
}
