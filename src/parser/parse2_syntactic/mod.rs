pub mod statement;

mod parser_statements;

use crate::error::CompilationError as CE;
use super::parse1_tokenize::token::TokenWithPos;
use statement::Statement;

pub fn parse_statements(tokens: Vec<TokenWithPos>) -> Result<Vec<Statement>, CE> {
    parser_statements::parse_statements(tokens)
}
