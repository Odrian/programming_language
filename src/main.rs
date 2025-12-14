use std::path::PathBuf;
use clap::Parser;
use programming_language::{Args, compile_src};
use programming_language::module_tree::ModuleTree;

fn main() {
    let args = Args::parse();
    let mut module_tree = ModuleTree::new();

    let path = PathBuf::from("src");
    let result = compile_src(&args, &mut module_tree, path);
    match result {
        Ok(()) => {
            println!("compiled");
        },
        Err(()) => {
            eprintln!("not compiled due to error");
        },
    }
}
