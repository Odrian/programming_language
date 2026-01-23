use super::common::*;

#[test]
fn test_struct_declarations() {
    assert_eq!(None, test_global("A :: struct {}").err());
    assert_eq!(None, test_global("A :: struct { x: i32 }").err());
    assert_eq!(None, test_global("A :: struct { x: i32, }").err());
    assert_eq!(None, test_global("A :: struct { x: i32, y: i32, }").err());

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
