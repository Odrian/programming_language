pub mod statement;

mod parser_statements;

mod error;

use crate::error::CResult;
use super::parse1_tokenize::token::RangedToken;
use statement::Statement;

pub fn parse_statements(tokens: Vec<RangedToken>) -> CResult<Vec<Statement>> {
    parser_statements::parse_statements(tokens)
        .map_err(|err| err.print())
}
