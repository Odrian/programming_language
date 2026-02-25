use clap::Parser;
use pr_compiler::{compile_src, Args};
use std::path::PathBuf;

fn main() {
    let args = Args::parse();

    let path = PathBuf::from("src");
    let result = compile_src(&args, path);
    match result {
        Ok(()) => {
            println!("compiled");
        },
        Err(()) => {
            eprintln!("not compiled due to error");
        },
    }
}
