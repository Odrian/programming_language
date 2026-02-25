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

    assert_eq!(5, get_exit_code_main("\
x := 3;
y := &&&&&x;
x = 5;
return *****y;
"));
    assert_eq!(5, get_exit_code_main("\
x := 123;
y := 5;
z := &&x;
*z = &y;
return **z;
"));
}

#[test]
fn test_reference() {
    assert_eq!(0, get_exit_code_main("\
x : i32 = 3;
y : &i32 = &x;
return 0;
"));

    assert_eq!(0, get_exit_code_main("\
x : i32 = 3;
y1 : &i32 = &x;
y2 : &&i32 = &y1;
y3 : &&&i32 = &y2;
return 0;
"));

    assert_eq!(5, get_exit_code_main("\
x : i32 = 3;
y : &i32 = &x;
y = 5;
return x;
"));

    assert_eq!(8, get_exit_code_main("\
x : i32 = 3;
y : &i32 = &x;
y += 5;
return x;
"));

    assert_eq!(5, get_exit_code("\
swap :: (x: &i32, y: &i32) {
    temp : i32 = x;
    x = y;
    y = temp;
}

main :: () -> i32 {
    x := 3; y := 5;
    swap(&x, &y);
    return x;
}
"));

//     assert_eq!(0, get_exit_code_main("\
// x := 0; y : &i32 = &x;
// if x == y || y == x || y == y {
//     return 0;
// }
// return 1;
// "));
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
