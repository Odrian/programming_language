use clap::Parser as _;
use pr_common::Target;
use pr_common::error::ErrorQueue;
use pr_compiler::{Args, CompileConfig, compile_src};
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
        }
        Err(_unit) => {
            eprintln!("not compiled due to error");
            std::process::exit(1);
        }
    }
}
