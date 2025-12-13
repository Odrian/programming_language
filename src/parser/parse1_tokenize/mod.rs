pub mod token;

mod parser_tokens;
mod error;

use crate::error::CResult;

pub fn tokenize(text: &str) -> CResult<Vec<token::TokenWithPos>> {
    parser_tokens::parse_tokens(text)
}
