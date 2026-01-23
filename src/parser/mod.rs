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

use crate::io_error::FileError;
use std::fs;
use std::path::PathBuf;

const ARTIFACT_DIR: &str = "artifacts";

pub fn parse_to_exe(args: &Args, file_path: PathBuf) -> CResult<()> {
    let filename = file_path.file_name().unwrap().to_str().unwrap().to_owned();
    let text = fs::read_to_string(file_path).expect("can't read file");

    let tokens = parse1_tokenize::tokenize(&text)?;
    if args.write_tokens_to_file {
        let text = tokens.iter()
            .map(|t| format!("{:#?}", t.token))
            .collect::<Vec<_>>().join("\n");

        fs::create_dir_all(ARTIFACT_DIR).unwrap();
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

        fs::create_dir_all(ARTIFACT_DIR).unwrap();
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

    // let mut object_factory = ObjectFactory::default();
    let linked_program = parse3_linking::link_all(args, statements)?;

    if args.write_syntactic_tree_to_file {
        let text = [&linked_program.type_statements, &linked_program.variable_statement, &linked_program.function_statement]
            .iter().map(|hashmap| hashmap.values()
                .map(|statement| statement.to_string())
                .collect::<Vec<_>>().join("\n")
        ).collect::<Vec<_>>().join("\n");
    
        fs::create_dir_all(ARTIFACT_DIR).unwrap();
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

    compiling::parse_to_llvm(args, linked_program)
        .map_err(|err| { println!("{err}"); })?;

    Ok(())
}
