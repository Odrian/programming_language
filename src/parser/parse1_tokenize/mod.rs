use crate::error::CompilationError as CE;

pub mod token;

mod parser_tokens;

use parser_tokens::parse_tokens;

pub fn tokenize(text: &[char]) -> Result<Vec<token::TokenWithPos>, CE> {
    parse_tokens(text)
}
