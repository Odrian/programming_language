use crate::error::CompilationError as CE;

mod syntactical;
mod tokenizer;

pub use tokenizer::PositionInFile;

pub fn parse(filepath: &str) -> Result<(), CE> {
    let tokens = tokenizer::tokenize(filepath)?;
    let statements = syntactical::parse(&tokens)?;

    for statement in statements {
        println!("{statement}");
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tokenizer::Token;
    use std::fs;
    use std::io::Write;

    fn tokenize_text(text: &str) -> Result<Vec<Token>, CE> {
        let path = "test_file";
        let mut file = fs::File::create(path).unwrap();
        file.write_all(text.as_bytes()).unwrap();
        let result = tokenizer::tokenize(path);
        let result = result.map(|v| v.into_iter().map(|t| t.token).collect());
        fs::remove_file(path).unwrap();
        result
    }

    #[test]
    fn test_tokens() {
        let text = "cat+323 dog=3";
        let expected = vec![
            Token::String("cat".to_string()),
            Token::Plus,
            Token::NumberLiteral("323".to_string()),
            Token::String("dog".to_string()),
            Token::Equal,
            Token::NumberLiteral("3".to_string()),
        ];
        let actual = tokenize_text(text);
        assert_eq!(actual.unwrap(), expected);
    }
}
