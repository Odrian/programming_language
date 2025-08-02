use crate::error::CompilationError as CE;

pub mod token2;

mod parser_tokens2;

use super::parse1_tokenize::token::TokenWithPos;
use super::parse2_brackets::token2::Token2WithPos;

pub fn parse_brackets(tokens: Vec<TokenWithPos>) -> Result<Vec<Token2WithPos>, CE> {
    parser_tokens2::parse_brackets(tokens)
}
