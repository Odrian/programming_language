use super::common::*;

#[test]
fn test_abs() {
    assert_eq!(2, get_exit_code("\
#extern
abs :: (i32) -> i32;

main :: () -> i32 {
    return abs(-2);
}
"))
}

#[test]
fn test_fprintf() {
    assert_eq!("Hello world!", run_code_stdout("\
FILE :: struct {}

#extern
stdout: *FILE;

#extern
fprintf :: (*FILE, *char);

main :: () -> i32 {
    fprintf(stdout, \"Hello world!\");
    return 0;
}

").unwrap())
}

#[test]
fn test_fprintf_with_vararg() {
    assert_eq!("765", run_code_stdout("\
FILE :: struct {}

#extern {
    stdout: *FILE;

    fprintf :: (*FILE, *char, ...);
}

main :: () -> i32 {
    fprintf(stdout, \"%d\", 765);
    return 0;
}

").unwrap())
}

