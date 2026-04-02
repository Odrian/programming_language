use lsp_types::{Position, Range};
use pr_common::{
    ranged_tree::{TreeBuilder, build, build_no_meta},
    BracketType,
    error::ErrorQueue,
    operations::*
};
use pr_tokenize::{token::*, tokenize, TokenLinearTree};


fn parse(text: &str) -> Result<TokenLinearTree, ErrorQueue> {
    let mut errors = ErrorQueue::default();
    let tree = tokenize(&mut errors, text);
    if errors.has_errors() {
        return Err(errors)
    }
    Ok(tree)
}

fn assert_has_error(str: &str) {
    assert_ne!(parse(str).err(), None);
}
fn assert_no_error(str: &str) {
    assert_eq!(parse(str).err(), None);
}
fn assert_result(str: &str, result: TokenLinearTree) {
    let actual = parse(str);
    let actual = match actual {
        Ok(actual) => actual,
        Err(err) => panic!("{err:?}"),
    };
    assert!(actual.eq_without_meta(&result), "assertion `left == right` failed\n  left = {0:?}\n right = {1:?}", &actual, &result);
}

#[test]
fn text_empty_string() {
    assert_result("", build_no_meta![]);
    assert_result(" ", build_no_meta![]);
}

#[test]
fn test_token_colon() {
    assert_result(
        ":::dog::cat: :dog::::",
        build_no_meta![
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
        ],
    );
}

#[test]
fn test_token_colon_equal() {
    assert_result(
        ":= dog ::= :==",
        build_no_meta![
            Token::EqualOperation(EqualOperation::ColonEqual),
            Token::String("dog".to_owned()),
            Token::DoubleColon,
            Token::EqualOperation(EqualOperation::Equal),
            Token::EqualOperation(EqualOperation::ColonEqual),
            Token::EqualOperation(EqualOperation::Equal),
        ],
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

    assert_result("()", build_no_meta![
        [ BracketType::Round;
        ]
    ]);
    assert_result("{}", build_no_meta![[BracketType::Curly; ]]);
    assert_result(
        ",+",
        build_no_meta![
            Token::Comma,
            NumberOperation::Add.into(),
        ],
    );

    assert_result(
        "{:=+cat=cat}",
        build_no_meta![
            [ BracketType::Curly;
                Token::EqualOperation(EqualOperation::ColonEqual),
                NumberOperation::Add.into(),
                Token::String("cat".to_owned()),
                Token::EqualOperation(EqualOperation::Equal),
                Token::String("cat".to_owned()),
            ]
        ],
    );
}

#[test]
fn test_tokens() {
    assert_result(
        "cat+323,cat=3d{,(,),}",
        build_no_meta![
            Token::String("cat".to_owned()),
            NumberOperation::Add.into(),
            Token::NumberLiteral("323".to_owned()),
            Token::Comma,
            Token::String("cat".to_owned()),
            Token::EqualOperation(EqualOperation::Equal),
            Token::NumberLiteral("3d".to_owned()),
            [ BracketType::Curly;
                Token::Comma,
                [ BracketType::Round;
                    Token::Comma,
                ]
                Token::Comma,
            ]
        ],
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

#[test]
fn test_correct_token_position() {
    assert_eq!(
        parse("a b\nc"),
        Ok(build![
            Token::String("a".to_string()), Range::new(Position::new(0, 0), Position::new(0, 1));
            Token::String("b".to_string()), Range::new(Position::new(0, 2), Position::new(0, 3));
            Token::String("c".to_string()), Range::new(Position::new(1, 0), Position::new(1, 1));
        ]),
    );

    assert_eq!(
        parse("\"a\nbb\""),
        Ok(build![
            Token::DoubleQuotes("a\nbb".to_string()), Range::new(Position::new(0, 0), Position::new(1, 3));
        ]),
    );
}
