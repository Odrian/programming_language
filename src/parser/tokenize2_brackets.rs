use std::fmt::Debug;
use super::tokenize1::{Token, TokenWithPos};
use crate::error::CompilationError as CE;
use crate::parser::PositionInFile;

pub fn parse_brackets(tokens: Vec<TokenWithPos>) -> Result<Vec<Token2WithPos>, CE> {
    let (token, end_index) = parse_inside_brackets(&tokens, 0, BracketType::None)?;
    if end_index != tokens.len() {
        unreachable!("")
    }
    let Token2::Bracket(boxed, _) = token.token else { unreachable!() };
    Ok(boxed)
}

#[derive(Eq, PartialEq)]
pub struct Token2WithPos {
    pub token: Token2,
    pub position: PositionInFile,
}
impl Token2WithPos {
    fn new(token: Token2, position: PositionInFile) -> Self {
        Self { token, position }
    }
}
impl Debug for Token2WithPos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.token)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Token2 {
    String(String),
    NumberLiteral(String),
    TwoSidedOperation(TwoSidedOperation),
    Bracket(Vec<Token2WithPos>, BracketType),
}
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum BracketType {
    Curly,
    Round,
    None, // used for parsing whole file like other brackets
    // Square,
    // Quotes
    // DoubleQuotes
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

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum TwoSidedOperation {
    Equal = 0,
    Plus = 1,
}

fn parse_inside_brackets(
    tokens: &Vec<TokenWithPos>,
    start_index: usize,
    open_bracket_type: BracketType,
) -> Result<(Token2WithPos, usize), CE> {
    let mut result_tokens = Vec::new();

    let mut index = start_index;
    while index < tokens.len() {
        let token = &tokens[index];
        let result_token = match &token.token {
            Token::String(s) => Token2::String(s.clone()),
            Token::NumberLiteral(s) => Token2::NumberLiteral(s.clone()),

            Token::Plus => Token2::TwoSidedOperation(TwoSidedOperation::Plus),
            Token::Equal => Token2::TwoSidedOperation(TwoSidedOperation::Equal),

            Token::CurlyBracketOpen | Token::RoundBracketOpen => {
                let bracket_type = BracketType::from_token(&token.token);
                let (new_token, new_index) =
                    parse_inside_brackets(tokens, index + 1, bracket_type)?;
                index = new_index - 1; // -1 because later will be 'index += 1'
                new_token.token
            }
            Token::CurlyBracketClose | Token::RoundBracketClose => {
                let bracket_type = BracketType::from_token(&token.token);
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

    if open_bracket_type == BracketType::None {
        let result_token = Token2::Bracket(result_tokens, BracketType::None);
        Ok((Token2WithPos::new(result_token, PositionInFile::default()), tokens.len()))
    } else {
        Err(CE::BracketNotClosed(
            tokens[start_index - 1].position,
            open_bracket_type,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn map_add_place(vec: Vec<Token2>) -> Vec<Token2WithPos> {
        vec.into_iter().map(|x| Token2WithPos::new(x, PositionInFile::default())).collect()
    }
    fn map_remove_place(vec: Vec<Token2WithPos>) -> Vec<Token2> {
        vec.into_iter().map(|x| x.token).collect()
    }
    fn parse(tokens: Vec<Token>) -> Result<Vec<Token2>, CE> {
        let tokens = tokens.into_iter().map(|x| TokenWithPos::new(x, PositionInFile::default())).collect();
        parse_brackets(tokens).map(map_remove_place)
    }
    fn bracket_token2(vec: Vec<Token2>, bracket_type: BracketType) -> Token2 {
        let vec = map_add_place(vec);
        Token2::Bracket(vec, bracket_type)
    }
    #[test]
    fn test_parse_brackets() {
        let string = String::from("cat");

        assert_ne!(parse(vec![Token::CurlyBracketOpen]).err(), None);
        assert_ne!(parse(vec![Token::CurlyBracketClose]).err(), None);
        assert_ne!(parse(vec![Token::CurlyBracketOpen, Token::RoundBracketClose]).err(), None);
        assert_ne!(parse(vec![Token::CurlyBracketOpen, Token::CurlyBracketClose, Token::RoundBracketClose]).err(), None);
        assert_ne!(parse(vec![Token::CurlyBracketOpen, Token::CurlyBracketClose, Token::RoundBracketOpen]).err(), None);

        assert_eq!(parse(vec![Token::CurlyBracketOpen, Token::CurlyBracketClose]),
            Ok(vec![Token2::Bracket(vec![], BracketType::Curly)])
        );
        assert_eq!(parse(vec![Token::RoundBracketOpen, Token::RoundBracketClose]),
            Ok(vec![Token2::Bracket(vec![], BracketType::Round)])
        );

        assert_eq!(parse(vec![Token::CurlyBracketOpen, Token::Plus, Token::String(string.clone()), Token::Equal, Token::String(string.clone()), Token::CurlyBracketClose]).unwrap(),
            vec![bracket_token2(vec![
                Token2::TwoSidedOperation(TwoSidedOperation::Plus),
                Token2::String(string.clone()),
                Token2::TwoSidedOperation(TwoSidedOperation::Equal),
                Token2::String(string.clone()),
            ], BracketType::Curly)]
        );
    }
}