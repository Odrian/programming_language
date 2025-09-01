use programming_language::error::CompilationError as CE;
use programming_language::parser::parse1_tokenize::{token::*, tokenize};
use programming_language::parser::*;
use programming_language::parser::operations::*;

fn map_remove_place(vec: Vec<TokenWithPos>) -> Vec<Token> {
    vec.into_iter().map(|x| x.token).collect()
}
fn parse(text: &str) -> Result<Vec<Token>, CE> {
    let tokens = tokenize(text);
    tokens.map(map_remove_place)
}
fn assert_has_error(str: &str) {
    assert_ne!(parse(str).err(), None);
}
fn assert_no_error(str: &str) {
    assert_eq!(parse(str).err(), None);
}
fn assert_result(str: &str, result: Result<Vec<Token>, CE>) {
    let actual = parse(str);
    assert!(token_equality(&actual, &result), "assertion `left == right` failed\n  left = {0:?}\n right = {1:?}", &actual, &result);
}

fn token_equality(token1: &Result<Vec<Token>, CE>, token2: &Result<Vec<Token>, CE>) -> bool {
    if token1.is_ok() != token2.is_ok() {
        return false;
    }
    let Ok(token1) = token1 else { unreachable!() };
    let Ok(token2) = token2 else { unreachable!() };
    if token1.len() != token2.len() {
        return false;
    }
    let mut pairs = token1.iter().zip(token2);
    pairs.all(|(token1, token2)| {
        let br1 = matches!(token1, Token::Bracket(_, _));
        let br2 = matches!(token2, Token::Bracket(_, _));
        if br1 != br2 {
            false
        } else if br1 {
            let Token::Bracket(vec1, t1) = token1 else {unreachable!()};
            let Token::Bracket(vec2, t2) = token2 else {unreachable!()};
            let vec1 = map_remove_place(vec1.clone());
            let vec2 = map_remove_place(vec2.clone());
            t1 == t2 && token_equality(&Ok(vec1), &Ok(vec2))
        } else {
            true
        }
    })
}

fn bracket_token(bracket_type: BracketType, vec: Vec<Token>) -> Token {
    let vec = vec
        .into_iter()
        .map(|x| TokenWithPos::new(x, PositionInFile::new(0, 0)))
        .collect();
    Token::Bracket(vec, bracket_type)
}

#[test]
fn text_empty_string() {
    assert_result("", Ok(vec![]));
    assert_result(" ", Ok(vec![]));
}

#[test]
fn test_token_colon() {
    assert_result(
        ":::dog::cat: :dog::::",
        Ok(vec![
            Token::DoubleColon,
            Token::Colon,
            Token::String("dog".to_owned()),
            Token::DoubleColon,
            Token::String("cat".to_owned()),
            Token::Colon,
            Token::Colon,
            Token::String("dog".to_owned()),
            Token::DoubleColon,
            Token::DoubleColon,
        ]),
    );
}

#[test]
fn test_token_colon_equal() {
    assert_result(
        ":= dog ::= :==",
        Ok(vec![
            Token::EqualOperation(EqualOperation::ColonEqual),
            Token::String("dog".to_owned()),
            Token::DoubleColon,
            Token::EqualOperation(EqualOperation::Equal),
            Token::EqualOperation(EqualOperation::ColonEqual),
            Token::EqualOperation(EqualOperation::Equal),
        ]),
    );
}

#[test]
fn test_parse_brackets() {
    assert_has_error("(");
    assert_has_error(")");
    assert_has_error("{");
    assert_has_error("}");
    assert_has_error("{)");
    assert_has_error("{})");
    
    assert_no_error("{({(())})}");

    assert_result("()", Ok(vec![Token::Bracket(vec![], BracketType::Round)]));
    assert_result("{}", Ok(vec![Token::Bracket(vec![], BracketType::Curly)]));
    assert_result(
        ",+",
        Ok(vec![
            Token::Comma,
            NumberOperation::Add.into(),
        ]),
    );

    assert_result(
        "{:=+cat=cat}",
        Ok(vec![bracket_token(
            BracketType::Curly,
            vec![
                Token::EqualOperation(EqualOperation::ColonEqual),
                NumberOperation::Add.into(),
                Token::String("cat".to_owned()),
                Token::EqualOperation(EqualOperation::Equal),
                Token::String("cat".to_owned()),
            ],
        )]),
    );
}

#[test]
fn test_tokens() {
    assert_result(
        "cat+323,cat=3d{,(,),}",
        Ok(vec![
            Token::String("cat".to_owned()),
            NumberOperation::Add.into(),
            Token::NumberLiteral("323".to_owned()),
            Token::Comma,
            Token::String("cat".to_owned()),
            Token::EqualOperation(EqualOperation::Equal),
            Token::NumberLiteral("3d".to_owned()),
            bracket_token(
                BracketType::Curly,
                vec![
                    Token::Comma,
                    bracket_token(BracketType::Round, vec![Token::Comma]),
                    Token::Comma,
                ],
            ),
        ]),
    );
}

#[test]
fn test_correct_names() {
    assert_has_error("?");
    assert_has_error("@");

    assert_no_error("name");
    assert_no_error("_name");
    assert_no_error("_");
}
