pub mod parse1_tokenize;
pub mod parse2_syntactic;
pub mod parse3_linking;
pub mod compiling;

mod position_in_file;
pub use position_in_file::PositionInFile;

mod bracket_type;
pub use bracket_type::BracketType;

pub mod operations;

use crate::error::CResult;
use crate::Config;
use crate::parser::parse3_linking::object::ObjectFactory;

use crate::io_error::FileError;
use std::fs;

const ARTIFACT_DIR: &str = "artifacts";

pub fn parse(filename: &str, text: String, config: &Config) -> CResult<()> {
    let tokens = parse1_tokenize::tokenize(&text)?;
    if config.write_tokens_to_file {
        let text = tokens.iter()
            .map(|t| format!("{:#?}", t.token))
            .collect::<Vec<_>>().join("\n");

        let filepath = format!("{}/{}_tokens.txt", ARTIFACT_DIR, filename);
        let write_result = fs::write(&filepath, text);
        if let Err(err) = write_result {
            FileError::CantWriteToFile {
                filepath,
                what: "tokens".to_owned(),
                io_error: err.to_string()
            }.print();
            return Err(())
        }
    }

    let statements = parse2_syntactic::parse_statements(tokens)?;
    if config.write_unlinked_syntactic_tree_to_file {
        let text = statements.iter().map(|s| s.to_string()).collect::<Vec<_>>().join("\n");

        let filepath = format!("{}/{}_unlinked_AST.txt", ARTIFACT_DIR, filename);
        let write_result = fs::write(&filepath, text);
        if let Err(err) = write_result {
            FileError::CantWriteToFile {
                filepath,
                what: "unlinked AST".to_owned(),
                io_error: err.to_string()
            }.print();
            return Err(())
        }
    }

    let mut object_factory = ObjectFactory::default();
    let linked_statement = parse3_linking::link_variables(statements, &mut object_factory)?;
    if config.write_syntactic_tree_to_file {
        let text = linked_statement.iter().map(|s| s.to_string()).collect::<Vec<_>>().join("\n");

        let filepath = format!("{}/{}_AST.txt", ARTIFACT_DIR, filename);
        let write_result = fs::write(&filepath, text);
        if let Err(err) = write_result {
            FileError::CantWriteToFile {
                filepath,
                what: "AST".to_owned(),
                io_error: err.to_string()
            }.print();
            return Err(());
        }
    }

    compiling::parse_to_llvm(&config, linked_statement, &object_factory)
        .map_err(|err| { println!("{err}"); () })?;

    Ok(())
}
