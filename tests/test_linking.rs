use programming_language::error::CompilationError as CE;
use programming_language::parser::*;
use programming_language::parser::parse3_linking::linked_statement::*;
use programming_language::parser::parse3_linking::object::*;
use programming_language::parser::operations::*;

fn parse(text: &str) -> Result<(Vec<LinkedStatement>, ObjectFactory), CE> {
    let tokens = parse1_tokenize::tokenize(text)?;
    let statements = parse2_syntactic::parse_statements(tokens)?;
    let mut object_factory = ObjectFactory::default();
    let linked_statements = parse3_linking::link_variables(statements, &mut object_factory)?;
    Ok((linked_statements, object_factory))
}
fn assert_has_error(str: &str) {
    assert_ne!(parse(str).err(), None);
}
fn assert_no_error(str: &str) {
    assert_eq!(parse(str).err(), None);
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
fn test_functions() {
    assert_no_error("a :: () { }");
    assert_no_error("a :: (arg1: i32) { }");
    assert_no_error("a :: (arg1: i32) { x := arg1 }");
    assert_no_error("a :: (arg1: i32, arg2: i32) { x := arg1 + arg2 }");

    assert_no_error("a :: () -> i32 { return 0 }");
    assert_no_error("a :: () { return }");

    assert_has_error("a :: () -> i32 { return }");
    assert_has_error("a :: () { return 0 }");

    assert_has_error("a :: (arg1) {  }");
    assert_has_error("a :: (arg1: i32) { x = arg1 }");
    assert_has_error("a :: () { a = x }");
    assert_has_error("a :: () { x := a }");
    assert_has_error("a :: () { } a :: () {}");

    assert_no_error("a :: () { a := 0; x := a }");
}

#[test]
fn test_function_with_while() {
    assert_has_error("a :: ()  {         if 0 != 0 { x := 0 } e := x }");
    assert_has_error("a :: (b: i32) {    if b != 0 { x := 0 } e := x }");
    assert_has_error("a :: ()  {      while 0 != 0 { x := 0 } e := x }");
    assert_has_error("a :: (b: i32) { while b != 0 { x := 0 } e := x }");

    let text = "a :: (b: i32) -> i32 { c := b; while c != 0 { c = b } return c }";
    let result = parse(text);
    let Ok((mut statements, object_factory)) = result else {
        let err = result.err().unwrap();
        panic!("parsing error: {err}");
    };
    assert_eq!(statements.len(), 1);
    let LinkedStatement::Function { object: function_object, args, returns, body } = statements.pop().unwrap() else { panic!() };

    assert!(matches!(returns, ObjType::Integer(IntObjType::I32)), "{returns:?}");

    let function_type = object_factory.get_type(function_object);
    assert!(matches!(function_type, ObjType::Function { .. }));
    let ObjType::Function { arguments, returns } = function_type else { unreachable!() };
    assert_eq!(arguments, &vec![ObjType::Integer(IntObjType::I32)]);
    assert_eq!(returns.as_ref(), &ObjType::Integer(IntObjType::I32));

    assert_eq!(args.len(), 1);
    let arg = args[0];
    assert_eq!(body.len(), 3);
    
    let mut body_iter = body.into_iter();

    let LinkedStatement::VariableDeclaration { object: var1, value: value1 } = body_iter.next().unwrap() else { panic!() };
    let LinkedExpression::Variable(value1) = value1.expr else { panic!() };

    let LinkedStatement::While { condition, body: mut while_body } = body_iter.next().unwrap() else { panic!() };
    let LinkedExpression::Operation(left, right, op) = condition.expr else { panic!() };
    let LinkedExpression::Variable(condition_var) = left.expr else { panic!() };
    
    let LinkedExpression::IntLiteral(zero, _) = right.expr else { panic!() };
    assert_eq!(zero, "0");

    assert_eq!(op, CompareOperator::NotEqual.into());

    let LinkedStatement::Return(option_return) = body_iter.next().unwrap() else { panic!() };
    let LinkedExpression::Variable(c_var) = option_return.as_ref().unwrap().expr else { panic!() };
    assert_eq!(object_factory.get_name(c_var), "c");

    assert_eq!(while_body.len(), 1);
    let LinkedStatement::SetVariable { object: var2, value: value2 } = while_body.pop().unwrap() else { panic!() };
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
    assert_no_error("a :: () {} a()");
    assert_no_error("a :: () {} b :: () { a() }");
    assert_no_error("a :: (a1: i32) {} a(0 + 0)");
    assert_no_error("a :: (a1: i32) {} a(0)");
    assert_no_error("a :: (a1: i32, a2: i32) {} a(0, 0)");
    assert_no_error("a :: (a1: i32, a2: i32, a3: i32) {} a(0, 0, 0)");

    assert_has_error("a :: (0) {}");
    assert_has_error("a :: (0: i32) {}");
    assert_has_error("b :: () { a() }    a :: () {}");

    assert_has_error("a :: () {} a(0)");
    assert_has_error("a :: (ar1: i32) {} a()");
    assert_has_error("a :: (ar1: i32) {} a(0, 0)");
    assert_has_error("a :: (ar1: i32, ar2: i32) {} a()");
    assert_has_error("a :: (ar1: i32, ar2: i32) {} a(0)");
    assert_has_error("a :: (ar1: i32, ar2: i32) {} a(0, 0, 0)");
}
