use std::process::Command;
use std::sync::atomic;
use programming_language::Config;

pub fn get_exit_code(text: &str) -> i32 {
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
    if let Err(err) = result {
        panic!("{err}")
    }

    let code = Command::new(&path).status();
    let is_deleted = Command::new("rm").arg(&path).status().unwrap().success();
    assert!(is_deleted, "{}", format!("can't delete {path}"));

    code.unwrap().code().unwrap()
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
