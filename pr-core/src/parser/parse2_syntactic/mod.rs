pub mod statement;

mod parser_statements;

mod error;

use crate::error::ErrorQueue;
use super::parse1_tokenize::token::RangedToken;
use statement::RStatement;

pub fn parse_statements(errors: &mut ErrorQueue, tokens: Vec<RangedToken>) -> Vec<RStatement> {
    parser_statements::parse_statements(errors, tokens)
}
