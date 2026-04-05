use clap::Parser;
use pr_compiler::{compile_file, Args};
use std::path::PathBuf;
use pr_common::error::ErrorQueue;

fn main() {
    let args = Args::parse();

    let path = PathBuf::from("src/main.pr");

    let mut errors = ErrorQueue::default();
    let result = compile_file(&mut errors, &args, path);
    errors.print();

    match result {
        Ok(_unit) => {
            println!("compiled");
        },
        Err(_unit) => {
            eprintln!("not compiled due to error");
        },
    }
}
