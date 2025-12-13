use std::env;
use std::process::exit;
use programming_language::{Config, entry_point};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("WRONG ARGUMENTS, USE: programming_language <filepath>");
        exit(1);
    }
    let input = args[1].clone();

    let output = "main".to_owned();
    let create_executable = true;

    let write_tokens_to_file = false;
    let write_syntactic_tree_to_file = false;
    let write_unlinked_syntactic_tree_to_file = false;
    let create_llvm_ir = false;
    let create_object = false;

    let config = Config {
        input, output,
        create_executable,
        write_tokens_to_file, write_syntactic_tree_to_file,
        write_unlinked_syntactic_tree_to_file,
        create_llvm_ir, create_object,
    };

    let result = entry_point(config);
    match result {
        Ok(()) => {
            println!("compiled")
        },
        Err(()) => {
            eprintln!("not compiled due to error")
        },
    }
}
