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
    let string_num = u64::from_le_bytes([b'H', b'e', b'l', b'l', b'o', b'!', 0, 0]);
    assert_eq!("Hello!", run_code_stdout(&("\
#extern
stdout: *i8;

#extern
fprintf :: (*i8, *i8);

main :: () -> i32 {
    str: u64 = ".to_string() + &string_num.to_string() + "_u64;
    fprintf(stdout, &str as *i8);
    return 0;
}

")).unwrap())
}

