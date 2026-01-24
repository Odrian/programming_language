use super::common::*;

#[test]
fn test_struct_declarations() {
    assert_eq!(None, test_global("A :: struct {}").err());
    assert_eq!(None, test_global("A :: struct { x: i32 }").err());
    assert_eq!(None, test_global("A :: struct { x: i32, }").err());
    assert_eq!(None, test_global("A :: struct { x: i32, y: i32, }").err());

    assert_ne!(None, test_global("A :: struct { x: u32, x: u32 }").err());

    assert_eq!(None, test_global("A :: struct { x: i32, y: i32, } B :: struct { a: A }").err());
    assert_eq!(None, test_global("A :: struct { x: i32, y: i32, } B :: struct { a: A }").err());

    assert_eq!(None, test_global("A :: struct { b: *B } B :: struct { a:  A }").err());
    assert_eq!(None, test_global("A :: struct { b: *B } B :: struct { a: *A }").err());
    assert_eq!(None, test_global("A :: struct { b:  B } B :: struct { a: *A }").err());

    assert_ne!(None, test_global("A :: struct { a: A }").err());
    assert_ne!(None, test_global("A :: struct { b:  B } B :: struct { a:  A }").err());

    assert_ne!(None, test_global("A :: struct { b: B }").err());
    assert_ne!(None, test_global("A :: struct { b: *B }").err());
    assert_ne!(None, test_global("B :: () {} A :: struct { b: *B }").err());
}

#[test]
fn test_simple_struct() {
    assert_eq!(15, get_exit_code("\
A :: struct {
    x: i32,
    y: i32,
}

main :: () -> i32 {
    a: A;
    a.x = 5;
    a.y = 10;
    x : i32 = a.x;
    y : i32 = a.y;
    return x + y;
}
"));

    assert_eq!(15, get_exit_code("\
A :: struct {
    x: i32,
    y: i32,
}

new_a :: () -> A {
    a: A;
    a.x = 5;
    a.y = 10;
    return a;
}

get_sum :: (a: A) -> i32 {
    x : i32 = a.x;
    y : i32 = a.y;
    return x + y;
}

main :: () -> i32 {
    return get_sum(new_a());
}
"));
}
