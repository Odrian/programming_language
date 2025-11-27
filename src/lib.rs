pub mod parser;

pub mod error;

use std::fs;
use std::path::{Path, PathBuf};
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

fn get_all_files(path: impl AsRef<Path>) -> Vec<PathBuf> {
    let Ok(entries) = fs::read_dir(path) else { return vec![] };
    entries.flatten().flat_map(|entry| {
        let Ok(metadata) = entry.metadata() else { return vec![] };
        if metadata.is_file() { return vec![entry.path()] };
        if metadata.is_dir() { return get_all_files(entry.path()) };
        vec![]
    }).collect()
}

pub fn entry_point(config: Config) -> Result<(), CE> {
    let files = get_all_files("src");
    println!("Compiling files:\n{}\n", files.iter().map(|x| x.to_str().unwrap().to_owned()).collect::<Vec<_>>().join("\n"));

    assert_eq!(files.len(), 1);

    let prelinked: Vec<_> = files.iter()
        .map(|file| {
            let file_text = read_file(file)?;
            let file_name = file.file_name().unwrap().to_str().unwrap();
            parser::parse(file_name, file_text, &config)
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok(())
}

fn read_file(path: &PathBuf) -> Result<String, CE> {
    let result = fs::read_to_string(path.clone());
    match result {
        Ok(text) => Ok(text),
        Err(error) => Err(CE::CantReadSourceFile {
            filepath: path.to_str().unwrap().to_owned(),
            io_error: error.to_string(),
        })
    }
}
