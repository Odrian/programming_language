use super::common::*;

#[test]
fn test_operation_order() {
    assert_eq!(5, get_exit_code_main_return("1 + 2 * 2"));
    assert_eq!(5, get_exit_code_main_return("2 * 2 + 1"));
    assert_eq!(6, get_exit_code_main_return("1 + 2 * 2 + 1"));
    assert_eq!(13, get_exit_code_main_return("2 * 2 + 3 * 3"));
    assert_eq!(22, get_exit_code_main_return("2 * 2 + 3 * 3 + 1 * 7 + 1 + 1"));

    assert_eq!(256 - 2, get_exit_code_main_return("-1 - 1"));
    assert_eq!(256 - 2, get_exit_code_main_return("-1 as i32 - 1"));
}

#[test]
fn test_function_signatures() {
    assert_no_error("main :: () -> i32 { return 0 }");
    assert_no_error("foo :: () -> void { return; } main :: () -> i32 { foo(); return 0 }");
    assert_no_error("foo :: () -> void {         } main :: () -> i32 { foo(); return 0 }");

    assert_has_error("");
    assert_has_error("foo :: () -> i32 { return 0 }");
    assert_has_error("main :: () -> i32 {}");
    assert_has_error("main :: () { return }");

    assert_has_error("main :: () -> i32 { return 0 } foo :: (a: void) {}");
    assert_has_error(
        "\
foo :: () {}
main :: () -> i32 {
    x := foo();
    return 0
}
",
    );

    assert_no_error(
        "\
foo :: () {}
main :: () -> i32 {
    foo();
    return 0
}
",
    );
}

#[test]
fn test_else_if_evaluation_order() {
    assert_eq!(
        56,
        get_exit_code(
            "probe :: (checks: *i32, result: bool) -> bool {
                *checks += 1;
                return result;
            }
            choose :: (first: bool, second: bool) -> i32 {
                checks := 0;
                value := 0;
                if probe(&checks, first) { value = 1; }
                else if probe(&checks, second) { value = 2; }
                else { value = 3; }
                return checks * 10 + value;
            }
            main :: () -> i32 {
                return choose(true, true) + choose(false, true) + choose(false, false);
            }"
        ),
    );
}

#[test]
fn test_if_else_nested_continuations() {
    assert_eq!(
        23,
        get_exit_code(
            "count :: (b: bool) -> i32 {
                x := 0;
                if b {
                    if false { return 99; }
                    while x < 2 { x += 1; }
                } else {
                    while x < 3 { x += 1; }
                    if false { return 98; }
                }
                return x;
            }
            main :: () -> i32 { return count(true) * 10 + count(false); }"
        ),
    );
}

#[test]
fn test_for() {
    assert_eq!(
        8,
        get_exit_code_main(
            "\
    a := 0;
    for i in 0..8 {
        a += 1;
    }
    return a;
"
        )
    );

    assert_eq!(
        9,
        get_exit_code_main(
            "\
    a := 0;
    for i in 0..=8 {
        a += 1;
    }
    return a;
"
        )
    );

    assert_eq!(
        9,
        get_exit_code_main(
            "\
    left := 0;
    right := 8;
    a := 0;
    for i in left..=right {
        a += 1;
    }
    return a;
"
        )
    );

    assert_eq!(
        9,
        get_exit_code_main(
            "\
    a := 0;
    for i in (8 - 8)..=(4 + 4) {
        a += 1;
    }
    return a;
"
        )
    );
}
