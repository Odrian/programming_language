pub mod statement;

mod parser_statements;

mod error;

use crate::error::CResult;
use super::parse1_tokenize::token::RangedToken;
use statement::RStatement;

pub fn parse_statements(tokens: Vec<RangedToken>) -> CResult<Vec<RStatement>> {
    parser_statements::parse_statements(tokens)
        .map_err(|err| err.print())
}
