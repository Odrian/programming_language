use std::process::Command;
use clap::builder::OsStr;
use clap::Parser;
use programming_language::Args;
use programming_language::error::CResult;

use programming_language::module_tree::ModuleTree;
use tempfile;

pub fn run_code(text: &str) -> CResult<i32> {
    let temp_dir = tempfile::tempdir().expect("can't create temp dir");

    let src_path = temp_dir.path().join("src");
    std::fs::create_dir(&src_path).expect("can't create src dir");
    let main_txt_path = src_path.join("main.txt");
    std::fs::write(main_txt_path, text).expect("can't write to temp file");

    let name = "main";
    let out_path = temp_dir.path().join(name);

    let args = Args::parse_from([&OsStr::from("binary.exe"), out_path.as_os_str()]);
    let mut module_tree = ModuleTree::new();
    let result = programming_language::compile_src(&args, &mut module_tree, src_path);
    
    match result {
        Ok(()) => {
            let code = Command::new(out_path).status();

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
