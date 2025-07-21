use crate::error::CompilationError as CE;

mod parse1_tokenize;
mod parse2_brackets;
mod parse3_syntactic;

pub use parse1_tokenize::PositionInFile;
pub use parse2_brackets::BracketType;

pub fn parse(filepath: &str, debug: bool) -> Result<(), CE> {
    let tokens = parse1_tokenize::tokenize_file(filepath)?;
    if debug {
        let tokens_: Vec<_> = tokens.iter().map(|t| t.token.clone()).collect();
        println!("{tokens_:?}")
    }
    let tokens2 = parse2_brackets::parse_brackets(tokens)?;
    if debug {
        println!("{tokens2:?}")
    }
    let statements = parse3_syntactic::parse_statements(&tokens2)?;

    for statement in statements {
        println!("{statement}");
    }

    Ok(())
}
