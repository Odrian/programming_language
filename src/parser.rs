use crate::error::CompilationError as CE;

mod tokenize3_syntactic;
mod tokenize2_brackets;
mod tokenize1;

pub use tokenize1::PositionInFile;
pub use tokenize2_brackets::BracketType;

const DEBUG: bool = false;

pub fn parse(filepath: &str) -> Result<(), CE> {
    let tokens = tokenize1::tokenize_file(filepath)?;
    if DEBUG {
        let tokens_: Vec<_> = tokens.iter().map(|t| t.token.clone()).collect();
        println!("{tokens_:?}")
    }
    let tokens2 = tokenize2_brackets::parse_brackets(tokens)?;
    if DEBUG {
        println!("{tokens2:?}")
    }
    let statements = tokenize3_syntactic::parse_statements(&tokens2)?;

    for statement in statements {
        println!("{statement}");
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tokenize3_syntactic::{Expression, Statement};
    use crate::parser::tokenize1::Token;

    fn tokenize_text(text: &str) -> Result<Vec<Token>, CE> {
        tokenize1::tokenize(text).map(|v| v.into_iter().map(|t| t.token).collect())
    }

    #[test]
    fn test_tokens() {
        let text = "cat+323 dog=3d";
        let expected = vec![
            Token::String("cat".to_string()),
            Token::Plus,
            Token::NumberLiteral("323".to_string()),
            Token::String("dog".to_string()),
            Token::Equal,
            Token::NumberLiteral("3d".to_string()),
        ];
        let actual = tokenize_text(text);
        assert_eq!(actual.unwrap(), expected);
    }
    #[test]
    fn test_statements() {
        fn parse(text: &str) -> Result<Vec<Statement>, CE> {
            let tokens = tokenize1::tokenize(text)?;
            let tokens2 = tokenize2_brackets::parse_brackets(tokens)?;
            let statements = tokenize3_syntactic::parse_statements(&tokens2)?;
            Ok(statements)
        }
        assert_eq!(parse("cat = cat").err(), None);
        assert_eq!(parse("cat = 4").err(), None);
        assert_eq!(parse("cat = cat + cat").err(), None);
        assert_eq!(parse("cat = cat + 4").err(), None);
        assert_eq!(parse("cat = 4 + cat").err(), None);
        assert_eq!(parse("cat = 4 + 4").err(), None);
        assert_eq!(parse("cat = cat + cat + cat").err(), None);
        assert_eq!(parse("cat = cat + cat + cat + cat").err(), None);

        assert_ne!(parse("4 = cat").err(), None);
        assert_ne!(parse("cat = cat = cat").err(), None);
        assert_ne!(parse("cat + cat = cat").err(), None);
        assert_ne!(parse("cat + 4 = cat").err(), None);
        assert_ne!(parse("cat + cat").err(), None);
        assert_ne!(parse("cat += cat").err(), None);
        assert_ne!(parse("cat =+ cat").err(), None);
        assert_ne!(parse("cat = +cat").err(), None);
        assert_ne!(parse("cat == cat").err(), None);

        assert_eq!(parse("cat = (4 + 4)").err(), None);
        assert_eq!(parse("cat = ((4 + 4))").err(), None);
        assert_eq!(parse("cat = 4 + (4 + 4)").err(), None);
        assert_eq!(parse("cat = (4 + 4) + 4").err(), None);
        assert_eq!(parse("cat = cat \n cat = cat").err(), None);

        assert_ne!(parse("(cat + cat) = cat").err(), None);
        assert_ne!(parse("(4 + 4) = 4").err(), None);

        let variable = Expression::Variable(String::from("cat"));
        assert_eq!(parse("cat = cat + (cat + cat)"),
            Ok(vec![Statement::SetVariable {
                expression1: variable.clone(),
                expression2: Expression::plus(
                    variable.clone(),
                    Expression::RoundBracket(Box::new(
                        Expression::plus(variable.clone(), variable.clone())
                    ))
                )
            }])
        )
    }
}
