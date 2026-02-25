use std::path::PathBuf;
use clap::Parser;
use pr_core::{Args, compile_src};

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
