pub mod parse1_tokenize;
pub mod parse2_syntactic;
pub mod parse3_linking;
pub mod compiling;

mod position_in_file;
pub use position_in_file::PositionInFile;

mod bracket_type;
pub use bracket_type::BracketType;

pub mod operations;

use crate::error::CompilationError as CE;
use crate::Config;
use crate::parser::parse3_linking::object::ObjectFactory;

use std::fs;

pub fn parse(text: String, config: Config) -> Result<(), CE> {
    let tokens = parse1_tokenize::tokenize(&text)?;
    if config.write_tokens_to_file {
        let text = tokens.iter()
            .map(|t| format!("{:#?}", t.token))
            .collect::<Vec<_>>().join("\n");

        let filepath = format!("{}_tokens.txt", config.output);
        let write_result = fs::write(&filepath, text);
        if let Err(err) = write_result {
            return Err(CE::CantWriteToFile {
                filepath,
                what: "tokens".to_owned(),
                io_error: err.to_string()
            })
        }
    }

    let statements = parse2_syntactic::parse_statements(tokens)?;
    if config.write_unlinked_syntactic_tree_to_file {
        let text = statements.iter().map(|s| s.to_string()).collect::<Vec<_>>().join("\n");

        let filepath = format!("{}_unlinked_AST.txt", config.output);
        let write_result = fs::write(&filepath, text);
        if let Err(err) = write_result {
            return Err(CE::CantWriteToFile {
                filepath,
                what: "unlinked AST".to_owned(),
                io_error: err.to_string()
            })
        }
    }

    let mut object_factory = ObjectFactory::default();
    let linked_statement = parse3_linking::link_variables(statements, &mut object_factory)?;
    if config.write_syntactic_tree_to_file {
        let text = linked_statement.iter().map(|s| s.to_string()).collect::<Vec<_>>().join("\n");

        let filepath = format!("{}_AST.txt", config.output);
        let write_result = fs::write(&filepath, text);
        if let Err(err) = write_result {
            return Err(CE::CantWriteToFile {
                filepath,
                what: "AST".to_owned(),
                io_error: err.to_string()
            })
        }
    }

    compiling::parse_to_llvm(&config, linked_statement, &object_factory)?;

    Ok(())
}
