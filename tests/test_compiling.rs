use std::default::Default;
use std::process::Command;
use std::sync::atomic;
use programming_language::parser::Config;

fn get_exit_code(text: &str) -> i32 {
    static COUNTER: atomic::AtomicU32 = atomic::AtomicU32::new(0);
    let id = COUNTER.fetch_add(1, atomic::Ordering::Relaxed);
    let name = format!("test{id}");
    let path = format!("./{name}");

    let config = Config {
        output: name,
        ..Default::default()
    };
    let chars = text.chars().collect::<Vec<_>>();
    programming_language::parser::parse(&chars, config).unwrap();
    
    let code = Command::new(&path).status();
    let is_deleted = Command::new("rm").arg(&path).status().unwrap().success();
    assert!(is_deleted, "{}", format!("can't delete {path}"));

    code.unwrap().code().unwrap()
}

#[test]
fn test_simplest() {
    assert_eq!(5, get_exit_code("\
main :: () -> i32 {
    return 5
}
"));

    assert_eq!(3, get_exit_code("\
main :: () -> i32 {
    return 1 + 2
}
"));

    assert_eq!(7, get_exit_code("\
main :: () -> i32 {
    a := 7
    return a
}
"));

    assert_eq!(9, get_exit_code("\
main :: () -> i32 {
    a := 7
    a = 9
    return a
}
"));

    assert_eq!(12, get_exit_code("\
foo :: () -> i32 {
    return 12
}

main :: () -> i32 {
    return foo()
}
"));
}

#[test]
fn test_sum() {
    let program = "\
sum :: (a: i32, b: i32) -> i32 {
    c := a + b
    return c
}

main :: () -> i32 {
    return sum(5, 9)
}
";
    let result = get_exit_code(program);
    assert_eq!(result, 14);
}

#[test]
fn test_fibonachi() {
    let program = "\
fibonachi :: (n: i32) -> i32 {
    f0 := 0
    f1 := 1
    while n {
        f0 = f0 + f1
        temp := f0
        f0 = f1
        f1 = temp
        n = n - 1
    }
    return f0
}

main :: () -> i32 {
    return fibonachi(10)
}
";
    let result = get_exit_code(program);
    assert_eq!(result, 55);
}
