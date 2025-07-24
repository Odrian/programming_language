use crate::error::CompilationError as CE;

mod parse1_tokenize;
mod parse2_brackets;
mod parse3_syntactic;
mod parse4_linking;

mod position_in_file;
pub use position_in_file::PositionInFile;

mod bracket_type;
pub use bracket_type::BracketType;

pub fn parse(text: &[char], debug: bool) -> Result<(), CE> {
    let tokens = parse1_tokenize::tokenize(text)?;
    if debug {
        let tokens: Vec<_> = tokens.iter().map(|t| t.token.clone()).collect();
        println!("tokens:");
        println!("{tokens:?}");
        println!();
    }
    let tokens2 = parse2_brackets::parse_brackets(tokens)?;
    if debug {
        let tokens2: Vec<_> = tokens2.iter().map(|t| t.token.clone()).collect();
        println!("tokens2:");
        println!("{tokens2:?}");
        println!();
    }
    let statements = parse3_syntactic::parse_statements(&tokens2)?;

    println!("statements:");
    for statement in &statements {
        println!("{statement}");
    }
    println!();

    let linked_statement = parse4_linking::link_variables(statements)?;

    println!("linked statements:");
    for linked_statement in &linked_statement {
        println!("{linked_statement}");
    }

    Ok(())
}
