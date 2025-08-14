use std::process::Command;
use std::sync::atomic;
use programming_language::Config;
use programming_language::error::CompilationError as CE;

pub fn run_code(text: &str) -> Result<i32, CE> {
    static COUNTER: atomic::AtomicU32 = atomic::AtomicU32::new(0);
    let id = COUNTER.fetch_add(1, atomic::Ordering::Relaxed);
    let name = format!("test{id}");
    let path = format!("./{name}");
    let config = Config {
        input: "".to_owned(),
        output: name,
        ..Default::default()
    };

    let result = programming_language::parser::parse(text.to_owned(), config);
    
    match result {
        Ok(()) => {
            let code = Command::new(&path).status();
            let is_deleted = Command::new("rm").arg(&path).status().unwrap().success();
            assert!(is_deleted, "{}", format!("can't delete {path}"));

            Ok(code.unwrap().code().unwrap_or(-1))
        }
        Err(err) => Err(err)
    }
}

pub fn get_exit_code(text: &str) -> i32 {
    let result = run_code(text);
    
    match result {
        Ok(value) => value,
        Err(err) => panic!("{err}")
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
