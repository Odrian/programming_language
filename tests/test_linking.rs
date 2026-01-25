use clap::builder::OsStr;
use clap::Parser;
use programming_language::error::CResult;
use programming_language::parser::operations::*;
use programming_language::parser::parse3_linking::linked_statement::*;
use programming_language::parser::parse3_linking::object::*;
use programming_language::parser::parse3_linking::LinkedProgram;
use programming_language::parser::*;
use programming_language::Args;

fn parse(text: &str) -> CResult<LinkedProgram> {
    let tokens = parse1_tokenize::tokenize(&text)?;
    let statements = parse2_syntactic::parse_statements(tokens)?;
    let args = Args::parse_from([&OsStr::from("test_binary.exe"), &OsStr::from("test.exe")]);
    let linked_program = parse3_linking::link_all(&args, statements)?;
    Ok(linked_program)
}
fn assert_has_error_global(str: &str) {
    assert_ne!(parse(str).err(), None);
}
fn assert_no_error_global(str: &str) {
    assert_eq!(parse(str).err(), None);
}

fn assert_has_error(str: &str) {
    let actual_text = format!("main :: () -> i32 {{ {str}; return 0; }}");
    assert_has_error_global(&actual_text);
}
fn assert_no_error(str: &str) {
    let actual_text = format!("main :: () -> i32 {{ {str}; return 0; }}");
    assert_no_error_global(&actual_text)
}

#[test]
fn test_variables() {
    assert_no_error("cat := 0");
    assert_no_error("cat := (0 + 0)");

    assert_has_error("cat = 0");
    assert_has_error("cat = (cat + 0)");
    assert_has_error("cat := cat");
    assert_has_error("dog := cat");
    assert_has_error("cat := (cat + 0)");

    assert_no_error("cat := 0; cat = 0");
    assert_no_error("cat := 0; cat := 0");
    assert_no_error("cat := 0; dog := cat");
    assert_no_error("cat := 0; dog := (cat + cat)");
    assert_no_error("cat := 0; dog := (cat + cat)");
}

#[test]
fn test_literals() {
    assert_no_error("a := true");
    assert_no_error("a := false");
    assert_no_error("a := 0");
    assert_no_error("a := 0.0");
    assert_no_error("a := 10000000000000.000000000000000000001");
    assert_no_error("a := 10000000000000000000000000000000000");
    assert_no_error("a := 10000000000000.000000000000000000001f32");
    assert_no_error("a := 10000000000000000000000000000000000usize");
    assert_no_error("a := 10000000000000000000000000000000000u128");

    assert_no_error("a := 1i8");
    assert_no_error("a := 1i16");
    assert_no_error("a := 1i32");
    assert_no_error("a := 1i64");
    assert_no_error("a := 1i128");
    assert_no_error("a := 1isize");

    assert_no_error("a := 1u8");
    assert_no_error("a := 1u16");
    assert_no_error("a := 1u32");
    assert_no_error("a := 1u64");
    assert_no_error("a := 1usize");

    assert_no_error("a := -1i128");
    assert_has_error("a := -1u128");

    assert_no_error("a := 1f32");
    assert_no_error("a := 1f64");

    assert_no_error("a := 1_000_000_000");
    assert_no_error("a := 1_000_000_000___.___0");
    assert_no_error("a := 1___i32");
    assert_no_error("a := 1___f64");

    assert_has_error("a := 1e5");

    assert_no_error("a := 'a'");
    assert_no_error("a := '/'");
    assert_no_error("a := '\\'");
    assert_no_error("a := '\"'");

    assert_has_error("a := '''");
    assert_has_error("a := '😀'");

    // assert_no_error("a := \"\"");
    // assert_no_error("a := \"text\"");
}

#[test]
fn test_compare() {
    assert_no_error("cat := 0 == 0");
    assert_no_error("cat := 0 != 0");
    assert_no_error("cat := 0 < 0");
    assert_no_error("cat := 0 > 0");
    assert_no_error("cat := 0 >= 0");
    assert_no_error("cat := 0 >= 0");
    assert_no_error("cat := 0i128 >= 0i128");
    assert_no_error("cat := 0u128 >= 0u128");

    assert_has_error("cat := 0 >= 0i8");
    assert_has_error("cat := 0 >= 0f32");
}

#[test]
fn test_operations() {
    fn assert_no_error_bool(string: &str) {
        assert_no_error(&format!("a := false; b := {string}"))
    }
    fn assert_no_error_int(string: &str) {
        assert_no_error(&format!("a : i32 = 0; b := {string}"))
    }

    assert_no_error_int("-a");
    assert_no_error_bool("!a");
    assert_no_error_bool("a && a");
    assert_no_error_bool("a || a");
    assert_no_error_bool("a || a && a || a");

    assert_no_error("a := 0.0 + 0.0");
    assert_no_error("a := 0.0 - 0.0");
    assert_no_error("a := 0.0 * 0.0");
    assert_no_error("a := 0.0 / 0.0");

    assert_has_error("a := 0.0 % 0.0");
}

#[test]
fn test_as() {
    let bool_type = ["bool"];
    let int_types = [
        "isize", "usize",
        "i8", "i16", "i32", "i64", "i128",
        "u8", "u16", "u32", "u64", "u128",
    ];
    let float_types = ["f32", "f64"];
    
    fn check_numbers(func: &dyn Fn(&str), from: &[&str], to: &[&str]) {
        for t1 in from {
            for t2 in to {
                func(&format!("a: {t1} = 0_{t1}; b: {t2} = a as {t2}"))
            }
        }
    }
    fn check_two_sided(func: &dyn Fn(&str), from: &[&str], to: &[&str]) {
        check_numbers(func, from, to);
        check_numbers(func, to, from);
    }

    check_numbers(&assert_no_error, &int_types, &int_types);
    check_numbers(&assert_no_error, &bool_type, &bool_type);
    check_numbers(&assert_no_error, &float_types, &float_types);

    check_numbers(&assert_no_error, &bool_type, &int_types);
    check_numbers(&assert_has_error, &int_types, &bool_type);

    check_two_sided(&assert_has_error, &float_types, &bool_type);
    check_two_sided(&assert_has_error, &float_types, &int_types);
}

#[test]
fn test_functions() {
    assert_no_error_global("a :: () { }");
    assert_no_error_global("a :: (arg1: i32) { }");
    assert_no_error_global("a :: (arg1: i32) { x := arg1 }");
    assert_no_error_global("a :: (arg1: i32, arg2: i32) { x := arg1 + arg2 }");

    assert_no_error_global("a :: () -> i32 { return 0 }");
    assert_no_error_global("a :: () { return }");

    assert_has_error_global("a :: () -> i32 { return }");
    assert_has_error_global("a :: () { return 0 }");

    assert_has_error_global("a :: (arg1) {  }");
    assert_has_error_global("a :: (arg1: i32) { x = arg1 }");
    assert_has_error_global("a :: () { a = x }");
    assert_has_error_global("a :: () { x := a }");
    assert_has_error_global("a :: () { } a :: () {}");

    assert_no_error_global("a :: () { a := 0; x := a }");
}

#[test]
fn test_function_with_while() {
    assert_has_error_global("a :: ()  {         if 0 != 0 { x := 0 } e := x }");
    assert_has_error_global("a :: (b: i32) {    if b != 0 { x := 0 } e := x }");
    assert_has_error_global("a :: ()  {      while 0 != 0 { x := 0 } e := x }");
    assert_has_error_global("a :: (b: i32) { while b != 0 { x := 0 } e := x }");

    let text = "a :: (b: i32) -> i32 { c := b; while c != 0 { c = b } return c }";
    let result = parse(text);
    let Ok(linked_program) = result else {
        panic!("parsing error");
    };
    assert_eq!(linked_program.function_statement.len(), 1);
    let (function_object, GlobalLinkedStatement::Function { args, returns, body }) = linked_program.function_statement.into_iter().next().unwrap() else { panic!() };

    assert!(matches!(returns, ObjType::DEFAULT_INTEGER), "{returns:?}");

    let function_type = linked_program.factory.get_type(function_object);
    assert!(matches!(function_type, ObjType::Function { .. }));
    let ObjType::Function { arguments, returns } = function_type else { unreachable!() };
    assert_eq!(arguments, &vec![ObjType::DEFAULT_INTEGER]);
    assert_eq!(returns.as_ref(), &ObjType::DEFAULT_INTEGER);

    assert_eq!(args.len(), 1);
    let arg = args[0];
    assert_eq!(body.len(), 3);
    
    let mut body_iter = body.into_iter();

    let LinkedStatement::VariableDeclaration { object: var1, value: value1 } = body_iter.next().unwrap() else { panic!() };
    let LinkedExpression::Variable(value1) = value1.expr else { panic!() };

    let LinkedStatement::While { condition, body: mut while_body } = body_iter.next().unwrap() else { panic!() };
    let LinkedExpression::Operation(left, right, op) = condition.expr else { panic!() };
    let LinkedExpression::Variable(condition_var) = left.expr else { panic!() };
    
    let LinkedExpression::Literal(LinkedLiteralExpression::IntLiteral(zero, _)) = right.expr else { panic!() };
    assert_eq!(zero, "0");

    assert_eq!(op, CompareOperator::NotEqual.into());

    let LinkedStatement::Return(option_return) = body_iter.next().unwrap() else { panic!() };
    let LinkedExpression::Variable(c_var) = option_return.as_ref().unwrap().expr else { panic!() };
    assert_eq!(linked_program.factory.get_name(c_var), "c");

    assert_eq!(while_body.len(), 1);
    let LinkedStatement::SetVariable { what, value: value2, op: None } = while_body.pop().unwrap() else { panic!() };
    let LinkedExpression::Variable(var2) = what.expr else { panic!() };
    let LinkedExpression::Variable(value2) = value2.expr else { panic!() };
    // arg = b
    // var1 = c
    // value1 = b
    // condition = c
    // var2 = c
    // value2 = b

    assert_eq!(arg, value1);
    assert_eq!(arg, value2);
    assert_eq!(var1, condition_var);
    assert_eq!(var1, var2);
}

#[test]
fn test_function_call() {
    fn cover(global: &str, local: &str) -> String {
        format!("{global} xxx :: () {{ {local} }}")
    }
    assert_no_error_global(&cover("a :: () {}", "a()"));
    assert_no_error_global(&cover("a :: (a1: i32) {}", "a(0 + 0)"));
    assert_no_error_global(&cover("a :: (a1: i32) {}", "a(0)"));
    assert_no_error_global(&cover("a :: (a1: i32, a2: i32) {}", "a(0, 0)"));
    assert_no_error_global(&cover("a :: (a1: i32, a2: i32, a3: i32) {}", "a(0, 0, 0)"));

    assert_has_error_global("a :: (0) {}");
    assert_has_error_global("a :: (0: i32) {}");

    assert_no_error_global("b :: () { a() }    a :: () {}");

    assert_has_error_global(&cover("a :: () {}", "a(0)"));
    assert_has_error_global(&cover("a :: (ar1: i32) {}", "a()"));
    assert_has_error_global(&cover("a :: (ar1: i32) {}", "a(0, 0)"));
    assert_has_error_global(&cover("a :: (ar1: i32, ar2: i32) {}", "a()"));
    assert_has_error_global(&cover("a :: (ar1: i32, ar2: i32) {}", "a(0)"));
    assert_has_error_global(&cover("a :: (ar1: i32, ar2: i32) {}", "a(0, 0, 0)"));
}
