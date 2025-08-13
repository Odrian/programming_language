pub mod parser;

pub mod error;

use std::fs;
use error::CompilationError as CE;

pub struct Config {
    pub input: String,
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
            input: "main.txt".to_owned(),
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

pub fn entry_point(config: Config) -> Result<(), CE> {
    let filetext = read_file(&config.input)?;
    parser::parse(filetext, config)
}

fn read_file(path: &str) -> Result<String, CE> {
    let result = fs::read_to_string(path);
    match result {
        Ok(text) => Ok(text),
        Err(error) => Err(CE::CantReadSourceFile {
            filepath: path.to_owned(),
            io_error: error.to_string(),
        })
    }
}

