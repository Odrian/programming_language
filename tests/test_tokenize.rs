use programming_language::error::CompilationError as CE;
use programming_language::parser::parse1_tokenize::{tokenize, token::*};

fn string_to_chars(s: &str) -> Vec<char> {
    s.chars().collect()
}
fn tokenize_text(text: &[char]) -> Result<Vec<Token>, CE> {
    tokenize(text).map(|v| v.into_iter().map(|t| t.token).collect())
}

#[test]
fn test_tokens() {
    let v_cat = string_to_chars("cat");
    let v_323 = string_to_chars("323");
    let v_3d = string_to_chars("3d");

    let text = string_to_chars("cat+323,cat=3d{({),}");
    let expected = Ok(vec![
        Token::String(&v_cat),
        Token::Plus,
        Token::NumberLiteral(&v_323),
        Token::Comma,
        Token::String(&v_cat),
        Token::Equal,
        Token::NumberLiteral(&v_3d),
        Token::CurlyBracketOpen,
        Token::RoundBracketOpen,
        Token::CurlyBracketOpen,
        Token::RoundBracketClose,
        Token::Comma,
        Token::CurlyBracketClose,
    ]);
    let actual = tokenize_text(&text);
    assert_eq!(actual, expected);
}
#[test]
fn test_token_colon() {
    let v_cat = &string_to_chars("cat");
    let v_dog = &string_to_chars("dog");

    let text = string_to_chars(":::dog::cat: :dog::::");
    let expected = Ok(vec![
        Token::DoubleColon,
        Token::Colon,
        Token::String(v_dog),
        Token::DoubleColon,
        Token::String(v_cat),
        Token::Colon,
        Token::Colon,
        Token::String(v_dog),
        Token::DoubleColon,
        Token::DoubleColon,
    ]);
    let actual = tokenize_text(&text);
    assert_eq!(actual, expected);
}
#[test]
fn test_token_colon_equal() {
    let v_dog = &string_to_chars("dog");

    let text = string_to_chars(":= dog ::= :==");
    let expected = Ok(vec![
        Token::ColonEqual,
        Token::String(v_dog),
        Token::DoubleColon,
        Token::Equal,
        Token::ColonEqual,
        Token::Equal,
    ]);
    let actual = tokenize_text(&text);
    assert_eq!(actual, expected);
}

#[test]
fn text_empty_string() {
    assert_eq!(tokenize_text(&[]), Ok(Vec::new()));
    assert_eq!(tokenize_text(&[' ']), Ok(Vec::new()));
}
