use std::process::{Command, Stdio};
use clap::builder::OsStr;
use clap::Parser;
use programming_language::Args;
use programming_language::error::CResult;

use tempfile::TempDir;

fn compile_text(text: &str) -> CResult<(TempDir, Command)> {
   let temp_dir = tempfile::tempdir().expect("can't create temp dir");

    let src_path = temp_dir.path().join("src");
    std::fs::create_dir(&src_path).expect("can't create src dir");
    let main_txt_path = src_path.join("main.txt");
    std::fs::write(main_txt_path, text).expect("can't write to temp file");

    let name = "main";
    let out_path = temp_dir.path().join(name);

    let args = Args::parse_from([&OsStr::from("binary.exe"), out_path.as_os_str()]);
    let result = programming_language::compile_src(&args, src_path);
    
    match result {
        Ok(()) => {
            let command = Command::new(out_path);
            Ok((temp_dir, command))
        }
        Err(err) => Err(err)
    }
}

pub fn run_code(text: &str) -> CResult<i32> {
    let (_temp_dir, mut command) = compile_text(text)?;

    let code = command.status().unwrap().code().unwrap_or(-1);
    Ok(code)
}

pub fn run_code_stdout(text: &str) -> CResult<String> {
    let (_temp_dir, mut command) = compile_text(text)?;

    let stdio = Stdio::piped();
    command.stdout(stdio);

    let output = command.output().unwrap();
    if output.status.code().unwrap() != 0 { return Err(()) }

    let out_text = String::from_utf8_lossy(&output.stdout);
    Ok(out_text.to_string())
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

pub fn test_global(global: &str) -> CResult<()> {
    let program = format!("\
main :: () -> i32 {{
    return 0;
}}

{global}
");
    run_code(&program).map(|_| ())
}
