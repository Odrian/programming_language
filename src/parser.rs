use crate::error::CompilationError as CE;

mod parse1_tokenize;
mod parse2_brackets;
mod parse3_syntactic;
mod parse4_linking;

pub use parse1_tokenize::PositionInFile;
pub use parse2_brackets::BracketType;

pub fn parse(text: &[char], debug: bool) -> Result<(), CE> {
    let tokens = parse1_tokenize::tokenize(text)?;
    if debug {
        let tokens: Vec<_> = tokens.iter().map(|t| t.token.clone()).collect();
        println!("{tokens:?}")
    }
    let tokens2 = parse2_brackets::parse_brackets(tokens)?;
    if debug {
        let tokens2: Vec<_> = tokens2.iter().map(|t| t.token.clone()).collect();
        println!("{tokens2:?}")
    }
    let statements = parse3_syntactic::parse_statements(&tokens2)?;

    println!("statements:");
    for statement in &statements {
        println!("{statement}");
    }

    let linked_statement = parse4_linking::link_variables(statements)?;

    println!("linked statements:");
    for linked_statement in &linked_statement {
        println!("{linked_statement}");
    }

    Ok(())
}
