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
use crate::Args;
use crate::parser::parse3_linking::object::ObjectFactory;

use crate::io_error::FileError;
use crate::parser::parse2_syntactic::statement::Statement;
use std::fs;
use crate::module_tree::{ModuleId, ModuleTree};

const ARTIFACT_DIR: &str = "artifacts";

pub fn parse_to_statements(args: &Args, tree: &mut ModuleTree, module_id: ModuleId) -> CResult<Vec<Statement>> {
    let metadata = tree.get_metadata(module_id);
    if !metadata.is_file { panic!("try to parse directory") }
    let filename = &metadata.name;

    let path = tree.get_full_path(module_id);
    let text = fs::read_to_string(path).expect("can't read file");

    let tokens = parse1_tokenize::tokenize(&text)?;
    if args.write_tokens_to_file {
        let text = tokens.iter()
            .map(|t| format!("{:#?}", t.token))
            .collect::<Vec<_>>().join("\n");

        let filepath = format!("{ARTIFACT_DIR}/{filename}_tokens.txt");
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
    if args.write_unlinked_syntactic_tree_to_file {
        let text = statements.iter().map(ToString::to_string).collect::<Vec<_>>().join("\n");

        let filepath = format!("{ARTIFACT_DIR}/{filename}_unlinked_AST.txt");
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
    Ok(statements)
}

pub fn parse_statements_single_file(args: &Args, filename: &str, statements: Vec<Statement>) -> CResult<()> {
    let mut object_factory = ObjectFactory::default();
    let linked_statement = parse3_linking::link_variables(statements, &mut object_factory)?;
    if args.write_syntactic_tree_to_file {
        let text = linked_statement.iter().map(ToString::to_string).collect::<Vec<_>>().join("\n");

        let filepath = format!("{ARTIFACT_DIR}/{filename}_AST.txt");
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

    compiling::parse_to_llvm(args, linked_statement, &object_factory)
        .map_err(|err| { println!("{err}"); })?;

    Ok(())
}
