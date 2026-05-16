use clap::Parser;
use pr_common::error::ErrorQueue;
use pr_common::Target;
use pr_compiler::{compile_src, Args, CompileConfig};
use std::path::PathBuf;

fn main() {
    let args = Args::parse();

    let path = PathBuf::from("src/");

    let errors = &mut ErrorQueue::default();
    let config = &CompileConfig {
        target: Target::get_current(),
        args,
    };
    let result = compile_src(errors, config, path);
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
