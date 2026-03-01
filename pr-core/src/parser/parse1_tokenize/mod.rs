pub mod token;

mod parser_tokens;
mod error;

use crate::error::CResult;
use token::RangedToken;

pub fn tokenize(text: &str) -> CResult<Vec<RangedToken>> {
    parser_tokens::parse_tokens(text)
}
