pub mod statement;

mod parser_statements;

use crate::error::CResult;
use super::parse1_tokenize::token::TokenWithPos;
use statement::Statement;

pub fn parse_statements(tokens: Vec<TokenWithPos>) -> CResult<Vec<Statement>> {
    parser_statements::parse_statements(tokens)
        .map_err(|err| { println!("{err}"); () })
}
