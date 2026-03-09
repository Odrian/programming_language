pub mod token;

mod parser_tokens;
mod error;

use crate::error::ErrorQueue;
use token::RangedToken;

pub fn tokenize(errors: &mut ErrorQueue, text: &str) -> Vec<RangedToken> {
    parser_tokens::parse_tokens(errors, text)
}
