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
    assert_has_error("\
foo :: () {}
main :: () -> i32 {
    x := foo();
    return 0
}
");

    assert_no_error("\
foo :: () {}
main :: () -> i32 {
    foo();
    return 0
}
");
}

#[test]
fn test_for() {
    assert_eq!(8, get_exit_code_main("\
    a := 0;
    for i in 0..8 {
        a += 1;
    }
    return a;
"));

    assert_eq!(9, get_exit_code_main("\
    a := 0;
    for i in 0..=8 {
        a += 1;
    }
    return a;
"));

    assert_eq!(9, get_exit_code_main("\
    left := 0;
    right := 8;
    a := 0;
    for i in left..=right {
        a += 1;
    }
    return a;
"));

    assert_eq!(9, get_exit_code_main("\
    a := 0;
    for i in (8 - 8)..=(4 + 4) {
        a += 1;
    }
    return a;
"));
}
