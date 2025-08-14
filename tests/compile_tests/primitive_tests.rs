use super::common::*;

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
fn test_literals() {
    assert_eq!(1, get_exit_code_main_return("257u8 as i32"));
    assert_eq!(0, get_exit_code_main_return("((-1 as u128) + 1_u128) as i32"));
    assert_eq!(1, get_exit_code_main_return(&format!("('a' as i32 == {}) as i32", 'a' as i32)));
    assert_eq!(1, get_exit_code_main_return(&format!("({}_u8 as char == 'a') as i32", 'a' as i32)));

    // FIXME: uncomment when fixed
//     get_exit_code_main("\
// a := 10000000000000000000000000000000000000000000.0000000000000000000000000000000000001_f32;
// a := 100000000000000000000000000000000000000000000000000000000000000000000000000000001_u8;
// return 0
// ");
}

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
fn test_float() {
    // TODO: test floats
}
