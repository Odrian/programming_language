use programming_language::parser::{parse, Config};
use std::{env, fs};
use programming_language::error::CompilationError as CE;

fn main() -> Result<(), CE> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(CE::WrongArguments("WRONG ARGUMENTS, USE: programming_language <filepath>".to_owned()))
    }
    let file_path = &args[1];

    let output = "main".to_owned();
    let create_executable = true;

    let write_tokens_to_file = false;
    let write_syntactic_tree_to_file = false;
    let write_unlinked_syntactic_tree_to_file = false;
    let create_llvm_ir = false;
    let create_object = false;

    let config = Config {
        output, create_executable,
        write_tokens_to_file, write_syntactic_tree_to_file,
        write_unlinked_syntactic_tree_to_file,
        create_llvm_ir, create_object,
    };

    let file_text = read_file(file_path)?;
    parse(file_text, config)?;

    Ok(())
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
