use std::process::Command;
use std::sync::atomic;
use clap::Parser;
use programming_language::Args;
use programming_language::error::CResult;

use tempfile;

pub fn run_code(text: &str) -> CResult<i32> {
    let temp_dir = tempfile::tempdir().expect("can't create temp dir");

    let name = "main";
    let path = temp_dir.path().join(name);

    let args = Args::parse_from(["binary.exe", path.to_str().unwrap()]);
    let result = programming_language::parser::parse("test", text.to_owned(), &args);
    
    match result {
        Ok(()) => {
            println!("{path:?}");
            let code = Command::new(path).status();

            Ok(code.unwrap().code().unwrap_or(-1))
        }
        Err(err) => Err(err)
    }
}

pub fn get_exit_code(text: &str) -> i32 {
    let result = run_code(text);

    match result {
        Ok(value) => value,
        Err(()) => panic!("not compiled")
    }
}

pub fn assert_has_error(text: &str) {
    let result = run_code(text);
    assert!(result.is_err())
}

pub fn assert_no_error(text: &str) {
    assert_eq!(Ok(0), run_code(text))
}

pub fn get_exit_code_main(text: &str) -> i32 {
    let program = "\
main :: () -> i32 {
    ".to_owned() + text + "
}
";
    get_exit_code(&program)
}

pub fn get_exit_code_main_return(text: &str) -> i32 {
    get_exit_code_main(&format!("return {text}"))
}
