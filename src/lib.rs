pub mod parser;

pub mod error;
pub mod io_error;

use std::fs;
use std::path::{Path, PathBuf};
use clap::Parser;
use error::CResult;
use io_error::FileError;

// TODO: implement different collect files strategies: src folder, only input files, input files + use statements

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct Args {
    pub output: String,

    #[arg(long, default_value_t = false)]
    pub dont_create_executable: bool,

    #[arg(long, default_value_t = false)]
    pub write_tokens_to_file: bool,
    #[arg(long, default_value_t = false)]
    pub write_unlinked_syntactic_tree_to_file: bool,
    #[arg(long, default_value_t = false)]
    pub write_syntactic_tree_to_file: bool,
    #[arg(long, default_value_t = false)]
    pub create_llvm_ir: bool,
    #[arg(long, default_value_t = false)]
    pub create_object: bool,
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

pub fn compile_src(args: Args) -> CResult<()> {
    let files = get_all_files("src");
    compile_files(args, files)
}

pub fn compile_files(args: Args, files: Vec<PathBuf>) -> CResult<()> {
    println!("Compiling files:\n{}\n", files.iter().map(|x| x.to_str().unwrap().to_owned()).collect::<Vec<_>>().join("\n"));

    if files.len() != 1 { unimplemented!("multiple files") }

    let prelinked: Vec<_> = files.iter()
        .map(|file| {
            let file_text = read_file(file)?;
            let file_name = file.file_name().unwrap().to_str().unwrap();
            parser::parse(file_name, file_text, &args)
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok(())
}

fn read_file(path: &PathBuf) -> CResult<String> {
    let result = fs::read_to_string(path.clone());
    match result {
        Ok(text) => Ok(text),
        Err(error) => {
            FileError::CantReadSourceFile {
                filepath: path.to_str().unwrap().to_owned(),
                io_error: error.to_string(),
            }.print();
            Err(())
        }
    }
}
