pub mod parse1_tokenize;
pub mod parse3_syntactic;
pub mod parse4_linking;
pub mod compiling;

mod position_in_file;
pub use position_in_file::PositionInFile;

mod bracket_type;
pub use bracket_type::BracketType;

use crate::error::CompilationError as CE;
use std::fs;

pub struct Config {
    pub output: String,
    pub create_executable: bool,

    pub write_tokens_to_file: bool,
    pub write_unlinked_syntactic_tree_to_file: bool,
    pub write_syntactic_tree_to_file: bool,
    pub create_llvm_ir: bool,
    pub create_object: bool,
}
impl Default for Config {
    fn default() -> Self {
        Self {
            output: "main".to_owned(),
            create_executable: true,

            write_tokens_to_file: false,
            write_unlinked_syntactic_tree_to_file: false,
            write_syntactic_tree_to_file: false,
            create_llvm_ir: false,
            create_object: false,
        }
    }
}

pub fn parse(text: &[char], config: Config) -> Result<(), CE> {
    let tokens = parse1_tokenize::tokenize(text)?;
    if config.write_tokens_to_file {
        let text = tokens.iter()
            .map(|t| format!("{:#?}", t.token))
            .collect::<Vec<_>>().join("\n");

        let filename = format!("{}_tokens.txt", config.output);
        fs::write(&filename, text).unwrap_or_else(|err|
            panic!("Can't write tokens to {filename}: {err}")
        );
    }

    let statements = parse3_syntactic::parse_statements(&tokens)?;
    if config.write_unlinked_syntactic_tree_to_file {
        let text = statements.iter().map(|s| s.to_string()).collect::<Vec<_>>().join("\n");

        let filename = format!("{}_unlinked_AST.txt", config.output);
        fs::write(&filename, text).unwrap_or_else(|err|
            panic!("Can't write unlinked_AST to {filename}: {err}")
        );
    }

    let linked_statement = parse4_linking::link_variables(&statements)?;
    if config.write_syntactic_tree_to_file {
        let text = linked_statement.iter().map(|s| s.to_string()).collect::<Vec<_>>().join("\n");

        let filename = format!("{}_AST.txt", config.output);
        fs::write(&filename, text).unwrap_or_else(|err|
            panic!("Can't write AST to {filename}: {err}")
        );
    }

    compiling::parse_to_llvm(&config, &linked_statement)?;

    Ok(())
}
