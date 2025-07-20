use crate::error::CompilationError as CE;

mod tokenize3_syntactic;
mod tokenize2_brackets;
mod tokenize1;

pub use tokenize1::PositionInFile;
pub use tokenize2_brackets::BracketType;

pub fn parse(filepath: &str, debug: bool) -> Result<(), CE> {
    let tokens = tokenize1::tokenize_file(filepath)?;
    if debug {
        let tokens_: Vec<_> = tokens.iter().map(|t| t.token.clone()).collect();
        println!("{tokens_:?}")
    }
    let tokens2 = tokenize2_brackets::parse_brackets(tokens)?;
    if debug {
        println!("{tokens2:?}")
    }
    let statements = tokenize3_syntactic::parse_statements(&tokens2)?;

    for statement in statements {
        println!("{statement}");
    }

    Ok(())
}
