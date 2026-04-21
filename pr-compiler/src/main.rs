use clap::Parser;
use pr_compiler::{compile_file, Args, CompileConfig};
use std::path::PathBuf;
use pr_common::error::ErrorQueue;
use pr_common::Target;

fn main() {
    let args = Args::parse();

    let path = PathBuf::from("src/main.pr");

    let errors = &mut ErrorQueue::default();
    let config = &CompileConfig {
        target: Target::get_current(),
        args,
    };
    let result = compile_file(errors, config, path);
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
