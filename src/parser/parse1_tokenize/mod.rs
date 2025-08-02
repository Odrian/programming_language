use crate::error::CompilationError as CE;

pub mod token;

mod parser_tokens;

use parser_tokens::split_text;

pub fn tokenize(text: &[char]) -> Result<Vec<token::TokenWithPos>, CE> {
    Ok(split_text(text))
}
