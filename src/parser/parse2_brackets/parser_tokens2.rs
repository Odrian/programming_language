use crate::error::CompilationError as CE;
use crate::parser::{PositionInFile, BracketType};

use super::super::parse1_tokenize::token::*;
use super::token2::*;

pub fn parse_brackets(tokens: Vec<TokenWithPos>) -> Result<Vec<Token2WithPos>, CE> {
    let (token, _) = parse_inside_brackets(&tokens, 0, None)?;
    let Token2::Bracket(vec, _) = token.token else { unreachable!() };
    Ok(vec)
}

pub fn parse_inside_brackets<'x>(
    tokens: &Vec<TokenWithPos<'x>>,
    start_index: usize,
    open_bracket_type: Option<BracketType>,
) -> Result<(Token2WithPos<'x>, usize), CE> {
    let mut result_tokens = Vec::new();

    let mut index = start_index;
    while index < tokens.len() {
        let token = &tokens[index];
        let result_token = match &token.token {
            Token::String(s) => Token2::String(s),
            Token::NumberLiteral(s) => Token2::NumberLiteral(s),

            Token::Comma => Token2::Comma,
            Token::Colon => Token2::Colon,
            Token::DoubleColon => Token2::DoubleColon,

            Token::Plus => Token2::TwoSidedOperation(TwoSidedOperation::Plus),

            Token::Equal => Token2::EqualOperation(EqualOperation::Equal),
            Token::ColonEqual => Token2::EqualOperation(EqualOperation::ColonEqual),

            Token::CurlyBracketOpen | Token::RoundBracketOpen => {
                let bracket_type = Some(BracketType::from_token(&token.token));
                let (new_token, new_index) =
                    parse_inside_brackets(tokens, index + 1, bracket_type)?;
                index = new_index - 1; // -1 because later will be 'index += 1'
                new_token.token
            }
            Token::CurlyBracketClose | Token::RoundBracketClose => {
                let bracket_type = BracketType::from_token(&token.token);
                if open_bracket_type.is_none() {
                    return Err(CE::BracketNotOpened(token.position, bracket_type));
                }
                let open_bracket_type = open_bracket_type.unwrap();

                if bracket_type != open_bracket_type {
                    return Err(CE::WrongBracketClosed {
                        start: tokens[start_index].position,
                        start_bracket_type: open_bracket_type,
                        end: token.position,
                        end_bracket_type: bracket_type,
                    });
                }
                let result_token = Token2::Bracket(result_tokens, bracket_type);
                return Ok((Token2WithPos::new(result_token, PositionInFile::default()), index + 1));
            }
        };
        result_tokens.push(Token2WithPos::new(result_token, token.position));
        index += 1;
    }

    if let Some(open_bracket_type) = open_bracket_type {
        Err(CE::BracketNotClosed(
            tokens[start_index - 1].position,
            open_bracket_type,
        ))
    } else {
        let unused_bracket_type = BracketType::Round;
        let unused_number = 0;
        let result_token = Token2::Bracket(result_tokens, unused_bracket_type);
        Ok((Token2WithPos::new(result_token, PositionInFile::default()), unused_number))
    }
}

impl BracketType {
    fn from_token(token: &Token) -> BracketType {
        match token {
            Token::CurlyBracketClose | Token::CurlyBracketOpen => BracketType::Curly,
            Token::RoundBracketClose | Token::RoundBracketOpen => BracketType::Round,
            _ => panic!("Unexpected token in bracket_type"),
        }
    }
}
