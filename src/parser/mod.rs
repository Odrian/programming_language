use crate::error::CompilationError as CE;

pub mod parse1_tokenize;
pub mod parse3_syntactic;
pub mod parse4_linking;

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

    let statements = parse3_syntactic::parse_statements(&tokens)?;

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
