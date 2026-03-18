pub mod token;

mod parser_tokens;
mod error;

use pr_common::error::ErrorQueue;
use token::RangedToken;

pub struct TokenizeResult {
    pub tokens: Vec<RangedToken>,
}

pub fn tokenize(errors: &mut ErrorQueue, text: &str) -> TokenizeResult {
    let tokens = parser_tokens::parse_tokens(errors, text);
    TokenizeResult { tokens }
}
