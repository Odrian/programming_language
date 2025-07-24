use crate::error::CompilationError as CE;

pub mod token2;

mod parser_tokens2;

use super::parse1_tokenize::token::TokenWithPos;
use super::parse2_brackets::token2::Token2WithPos;

pub fn parse_brackets(tokens: Vec<TokenWithPos>) -> Result<Vec<Token2WithPos>, CE> {
    parser_tokens2::parse_brackets(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::parse1_tokenize::token::*;
    use super::token2::*;
    use crate::parser::{PositionInFile, BracketType};

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
        assert_eq!(parse(vec![]).err(), None);
        assert_eq!(parse(vec![Token::Equal, Token::ColonEqual, Token::Comma]).err(), None);

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
        assert_eq!(parse(vec![Token::Comma, Token::Plus]),
           Ok(vec![Token2::Comma, Token2::TwoSidedOperation(TwoSidedOperation::Plus)])
        );

        let v_cat = &"cat".chars().collect::<Vec<_>>();
        assert_eq!(
            parse(vec![
                Token::CurlyBracketOpen,
                Token::ColonEqual,
                Token::Plus,
                Token::String(v_cat),
                Token::Equal,
                Token::String(v_cat),
                Token::CurlyBracketClose
            ]),
            Ok(vec![bracket_token2(vec![
                Token2::EqualOperation(EqualOperation::ColonEqual),
                Token2::TwoSidedOperation(TwoSidedOperation::Plus),
                Token2::String(v_cat),
                Token2::EqualOperation(EqualOperation::Equal),
                Token2::String(v_cat),
            ], BracketType::Curly)])
        );
    }
}
