use std::default::Default;
use std::process::Command;
use std::sync::atomic;
use programming_language::Config;

fn get_exit_code(text: &str) -> i32 {
    static COUNTER: atomic::AtomicU32 = atomic::AtomicU32::new(0);
    let id = COUNTER.fetch_add(1, atomic::Ordering::Relaxed);
    let name = format!("test{id}");
    let path = format!("./{name}");

    let config = Config {
        input: "".to_owned(),
        output: name,
        ..Default::default()
    };
    programming_language::parser::parse(text.to_owned(), config).unwrap();
    
    let code = Command::new(&path).status();
    let is_deleted = Command::new("rm").arg(&path).status().unwrap().success();
    assert!(is_deleted, "{}", format!("can't delete {path}"));

    code.unwrap().code().unwrap()
}

fn get_exit_code_main(text: &str) -> i32 {
    let program = "\
main :: () -> i32 {
    ".to_owned() + text + "
}
";
    get_exit_code(&program)
}

fn get_exit_code_main_return(text: &str) -> i32 {
    get_exit_code_main(&format!("return {text}"))
}

#[test]
fn test_simplest() {
    assert_eq!(5, get_exit_code_main_return("5"));

    assert_eq!(3, get_exit_code_main_return("1 + 2"));

    assert_eq!(7, get_exit_code_main("\
a := 7;
return a
"));

    assert_eq!(9, get_exit_code_main("\
a := 7;
a = 9;
return a
"));

    assert_eq!(12, get_exit_code("\
foo :: () -> i32 {
    return 12
}

main :: () -> i32 {
    return foo()
}
"));

    assert_eq!(10, get_exit_code_main_return("8|3&6"));
    assert_eq!(10, get_exit_code_main_return("3&6|8"));
}

#[test]
fn test_sum() {
    let program = "\
sum :: (a: i32, b: i32) -> i32 {
    c := a + b;
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
fn text_return_in_while() {
    let program = "\
foo :: (n: i32) -> i32 {
    while n > 0 {
        if n != 5 {
            return 10
        }
        n = n - 1
    }
    return n
}

main :: () -> i32 {
    return foo(7)
}
";
    let result = get_exit_code(program);
    assert_eq!(result, 10);
}

#[test]
fn test_fibonachi() {
    let program = "\
fibonachi :: (n: i32) -> i32 {
    f0 := 0;
    f1 := 1;
    while n > 0 {
        f0 = f0 + f1;
        temp := f0;
        f0 = f1;
        f1 = temp;
        n = n - 1;
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

#[test]
fn test_operation_order() {
    assert_eq!(5, get_exit_code_main_return("1 + 2 * 2"));
    assert_eq!(5, get_exit_code_main_return("2 * 2 + 1"));
    assert_eq!(6, get_exit_code_main_return("1 + 2 * 2 + 1"));
    assert_eq!(13, get_exit_code_main_return("2 * 2 + 3 * 3"));
    assert_eq!(22, get_exit_code_main_return("2 * 2 + 3 * 3 + 1 * 7 + 1 + 1"));

    assert_eq!(256 - 1, get_exit_code_main_return("-1 - 1"));
}

#[test]
fn test_compare() {
    let program = "\
foo :: (x: i32) -> i32 {
    if x == 2 { x = x + 4 }
    if x > 1 { x = x * 3 }
    if x < 2 { x = x - 1 }
    if x != 0 { x = x - 17 }
    if x >= 1 { x = x + 1 }
    if x <= 1 { x = x + 1 }
    return x
}
main :: () -> i32 {
    return foo(1) + foo(2)
}
";
    assert_eq!(1 + 2, get_exit_code(program))
}

#[test]
fn test_bool_op() {
    let program = "\
foo :: (x: i32, y: i32) -> i32 {
    if x == 1 && y == 1 { return 3 }
    if x == 1 { return 2 }
    if y == 1 { return 1 }
    return 0
}
main :: () -> i32 {
    return foo(1, 1) * foo(1, 8) + foo(9, 1) + 5 * foo(10, 10)
}
";
    assert_eq!(3 * 2 + 1, get_exit_code(program));
    
    assert_eq!(3, get_exit_code_main("if true  { return 3 } return 4"));
    assert_eq!(4, get_exit_code_main("if false { return 3 } return 4"));
}

#[test]
fn test_equal_set() {
    let program = "\
foo :: (n: i32) -> i32 {
    n /= 2;
    n -= 4;
    n += 1;
    n *= 9;
    n %= 8;
    return n
}
main :: () -> i32 {
    return foo(8)
}
";
    assert_eq!(1, get_exit_code(program));
}

#[test]
fn test_float() {
    // TODO: test floats
}

// FIXME: uncomment when fixed
/*
#[test]
fn test_max_u128() {
    let program = "\
main :: () -> i32 {
    x := 340282366920938463463374607431768211455_u128;
    count := 0;
    while x > 0_u128 {
        count += 1;
        x /= 10_u128;
    }
    return count;
}
";
    let actual_count = "340282366920938463463374607431768211455".chars().count();
    assert_eq!(actual_count as i32, get_exit_code(program));
}
*/
#[test]
fn test_max_u64() {
    let program = "\
main :: () -> i32 {
    x := 18446744073709551615_u64;
    count := 0;
    while x > 0_u64 {
        count += 1;
        x /= 10_u64;
    }
    return count;
}
";
    let actual_count = "18446744073709551615".replace('_', "").chars().count();
    assert_eq!(actual_count as i32, get_exit_code(program));
}

#[test]
fn test_u8() {
    let program = "\
main :: () -> i32 {
    return 257u8 as i32
}
";
    assert_eq!(1, get_exit_code(program));
}

// #[test]
// fn test_literals() {
//     get_exit_code_main("\
// a := 10000000000000000000000000000000000000000000.0000000000000000000000000000000000001_f32;
// a := 100000000000000000000000000000000000000000000000000000000000000000000000000000001_u8;
// return 0
// ");
// }
