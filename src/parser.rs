use crate::error::CompilationError as CE;

mod syntactical;
mod tokenizer;

pub use tokenizer::PositionInFile;

pub fn parse(filepath: &str) -> Result<(), CE> {
    let tokens = tokenizer::tokenize_file(filepath)?;
    let statements = syntactical::parse(&tokens)?;

    for statement in statements {
        println!("{statement}");
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::syntactical::Statement;
    use crate::parser::tokenizer::Token;

    fn tokenize_text(text: &str) -> Result<Vec<Token>, CE> {
        tokenizer::tokenize(text).map(|v| v.into_iter().map(|t| t.token).collect())
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
            let tokens = tokenizer::tokenize(text)?;
            let statements = syntactical::parse(&tokens)?;
            Ok(statements)
        }
        assert!(parse("cat = cat").is_ok());
        assert!(parse("cat = 4").is_ok());

        assert!(parse("4 = cat").is_err());
        assert!(parse("cat = cat = cat").is_err());
    }
}
