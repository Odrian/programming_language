use programming_language::parse;
use std::env;
use programming_language::error::CompilationError as CE;

fn main() -> Result<(), CE> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("WRONG ARGUMENTS, USE: programming_language <filepath>");
    }
    let filepath = &args[1];

    let debug = false;
    parse(filepath, debug)?;

    Ok(())
}
