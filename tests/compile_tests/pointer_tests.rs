use super::common::*;

#[test]
fn test_simplests() {
    assert_eq!(5, get_exit_code_main_return("*&5"));
    assert_eq!(5, get_exit_code_main("\
x := 3;
y := &x;
*y = 5;
return x;
"));
    assert_eq!(5, get_exit_code_main("\
x := 3;
y := &x;
x = 5;
return *y;
"));
}

#[test]
fn test_with_functions() {
    assert_eq!(5, get_exit_code("\
foo :: (arg1: *i32) {
    *arg1 = 5;
}
main :: () -> i32 {
    x := 0;
    foo(&x);
    return x
}
"));

    assert_eq!(5, get_exit_code("\
foo :: (arg1: *i32) -> i32 {
    return *arg1
}
main :: () -> i32 {
    return foo(&5)
}
"));
}

#[test]
fn test_int_casts() {
    assert_eq!(35, get_exit_code("\
main :: () -> i32 {
    x := &(256_u16 * 7_u16 + 5_u16);
    y1 := x as *u8;
    y2 := (y1 as usize + 1_usize) as *u8;
    return (*y1 * *y2) as i32
}
"))
}
