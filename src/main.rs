use programming_language::parse;
use std::env;
use programming_language::error::CompilationError as CE;

fn main() -> Result<(), CE> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("USAGE: programming_language <filepath>, you wrote {args:?}");
    }
    let filepath = &args[1];

    parse(&filepath)?;

    Ok(())
}
