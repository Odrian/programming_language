use clap::Parser;
use programming_language::{Args, compile_src};

fn main() {
    let args = Args::parse();

    let result = compile_src(args);
    match result {
        Ok(()) => {
            println!("compiled")
        },
        Err(()) => {
            eprintln!("not compiled due to error")
        },
    }
}
