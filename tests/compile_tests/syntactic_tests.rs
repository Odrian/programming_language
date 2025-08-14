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
