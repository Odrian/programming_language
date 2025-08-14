use super::common::*;

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
