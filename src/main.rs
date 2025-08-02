use programming_language::parser::parse;
use std::{env, fs};
use programming_language::error::CompilationError as CE;

fn main() -> Result<(), CE> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("WRONG ARGUMENTS, USE: programming_language <filepath>");
    }
    let file_path = &args[1];
    let file_text = read_file(file_path);

    let debug = true;
    parse(&file_text, debug)?;

    Ok(())
}

fn read_file(path: &str) -> Vec<char> {
    let result = fs::read_to_string(path);
    match result {
        Ok(text) => text.chars().collect(),
        Err(error) => {
            panic!("{error:?}");
        }
    }
}
