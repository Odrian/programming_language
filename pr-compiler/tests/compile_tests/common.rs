use std::process::{Command, ExitStatus, Stdio};
use clap::builder::OsStr;
use clap::Parser;
use pr_compiler::{Args, compile_file, CompileConfig};
use pr_common::error::{DiagnosticString, ErrorQueue};
use pr_common::Target;

use tempfile::TempDir;

fn compile_text(text: &str) -> Result<(TempDir, Command), ErrorQueue> {
   let temp_dir = tempfile::tempdir().expect("can't create temp dir");

    let main_txt_path = temp_dir.path().join("main.pr");
    std::fs::write(&main_txt_path, text).expect("can't write to temp file");

    let name = "main";
    let out_path = temp_dir.path().join(name);

    let args = Args::parse_from([&OsStr::from("binary.exe"), out_path.as_os_str()]);
    let config = &CompileConfig {
        target: Target::get_current(),
        args,
    };
    let mut errors = ErrorQueue::default();
    let result = compile_file(&mut errors, config, main_txt_path);

    match result {
        Ok(()) => {
            let command = Command::new(out_path);
            Ok((temp_dir, command))
        }
        Err(()) => Err(errors)
    }
}

fn unwrap_code(exit_status: &ExitStatus) -> Result<i32, ErrorQueue> {
    let Some(code) = exit_status.code() else {
        return Err(ErrorQueue::new_single_diag(DiagnosticString::from_text(
            "process was terminated by a signal"
        ).to_diag0()))
    };
    Ok(code)
}

pub fn run_code(text: &str) -> Result<i32, ErrorQueue> {
    let (_temp_dir, mut command) = compile_text(text)?;

    let status = command.status().unwrap();
    let code = unwrap_code(&status)?;
    Ok(code)
}

pub fn run_code_stdout(text: &str) -> Result<String, ErrorQueue> {
    let (_temp_dir, mut command) = compile_text(text)?;

    let stdio = Stdio::piped();
    command.stdout(stdio);

    let output = command.output().unwrap();
    let code = unwrap_code(&output.status)?;
    if code != 0 {
        return Err(ErrorQueue::new_single_diag(DiagnosticString::new(format!(
            "exit code {code}"
        )).to_diag0()))
    }

    let out_text = String::from_utf8_lossy(&output.stdout);
    Ok(out_text.to_string())
}

pub fn get_exit_code(text: &str) -> i32 {
    let result = run_code(text);

    match result {
        Ok(value) => value,
        Err(err) => panic!("{err:?}")
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

pub fn test_global(global: &str) -> Result<(), ErrorQueue> {
    let program = format!("\
main :: () -> i32 {{
    return 0;
}}

{global}
");
    run_code(&program).map(|_| ())
}
