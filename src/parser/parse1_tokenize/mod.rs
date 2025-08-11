pub mod token;

mod parser_tokens;

use crate::error::CompilationError as CE;

pub fn tokenize(text: &str) -> Result<Vec<token::TokenWithPos>, CE> {
    parser_tokens::parse_tokens(text)
}
