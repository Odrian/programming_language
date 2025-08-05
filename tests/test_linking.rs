use programming_language::error::CompilationError as CE;
use programming_language::parser::*;
use programming_language::parser::parse3_linking::linked_statement::*;

fn parse(text: &[char]) -> Result<Vec<LinkedStatement>, CE> {
    let tokens = parse1_tokenize::tokenize(text)?;
    let statements = parse2_syntactic::parse_statements(&tokens)?;
    let linked_statements = parse3_linking::link_variables(&statements)?;
    Ok(linked_statements)
}
fn string_to_chars(s: &str) -> Vec<char> {
    s.chars().collect()
}
fn assert_has_error(str: &str) {
    assert_ne!(parse(&string_to_chars(str)).err(), None);
}
fn assert_no_error(str: &str) {
    assert_eq!(parse(&string_to_chars(str)).err(), None);
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

    assert_no_error("cat := 0 cat = 0");
    assert_no_error("cat := 0 cat := 0");
    assert_no_error("cat := 0 dog := cat");
    assert_no_error("cat := 0 dog := (cat + cat)");
    assert_no_error("cat := 0 dog := (cat + cat)");
}

#[test]
fn test_functions() {
    assert_no_error("a :: () { }");
    assert_no_error("a :: (arg1) { }");
    assert_no_error("a :: (arg1) { x := arg1 }");
    assert_no_error("a :: (arg1, arg2) { x := arg1 + arg2 }");

    assert_has_error("a :: (arg1) { x = arg1 }");
    assert_has_error("a :: () { a = x }");
    assert_has_error("a :: () { x := a }");

    assert_no_error("a :: () { a := 0 x := a }");
}

#[test]
fn test_function_with_while() {
    assert_has_error("a :: ()  { if 0    { x := 0 } e := x }");
    assert_has_error("a :: (b) { if b    { x := 0 } e := x }");
    assert_has_error("a :: ()  { while 0 { x := 0 } e := x }");
    assert_has_error("a :: (b) { while b { x := 0 } e := x }");

    let text = string_to_chars("a :: (b) { c := b while c { c = b } }");
    let result = parse(&text);
    let Ok(statements) = result else {
        let err = result.err().unwrap();
        panic!("parsing error: {err}");
    };
    assert_eq!(statements.len(), 1);
    let LinkedStatement::Function { object: function_object, args, body } = &statements[0] else {
        panic!("expected function statement");
    };
    assert_eq!(function_object.obj_type, ObjType::Function { argument_count: 1 });
    assert_eq!(args.len(), 1);
    let arg = &args[0];
    assert_eq!(body.len(), 2);

    let LinkedStatement::VariableDeclaration { object: var1, value: value1 } = &body[0] else {
        panic!("expected variable declaration");
    };
    let LinkedExpression::Variable(value1) = value1 else {
        panic!("expected variable declaration");
    };

    let LinkedStatement::While { condition, body: while_body } = &body[1] else {
        panic!("expected while statement");
    };
    let LinkedExpression::Variable(condition) = condition else {
        panic!("expected variable declaration");
    };
    assert_eq!(while_body.len(), 1);
    let LinkedStatement::SetVariable { object: var2, value: value2 } = &while_body[0] else {
        panic!("expected variable declaration");
    };
    let LinkedExpression::Variable(value2) = value2 else {
        panic!("expected variable declaration");
    };
    // arg = b
    // var1 = c
    // value1 = b
    // condition = c
    // var2 = c
    // value2 = b

    assert_eq!(arg, value1);
    assert_eq!(arg, value2);
    assert_eq!(var1, condition);
    assert_eq!(var1, var2);
}

#[test]
fn test_function_call() {
    assert_no_error("a :: () {} a()");
    assert_no_error("a :: () {} b :: () { a() }");
    assert_no_error("a :: (a1) {} a(0 + 0)");
    assert_no_error("a :: (a1) {} a(0)");
    assert_no_error("a :: (a1, a2) {} a(0, 0)");
    assert_no_error("a :: (a1, a2, a3) {} a(0, 0, 0)");

    assert_has_error("a :: (0) {}");
    assert_has_error("b :: () { a() }    a :: () {}");

    assert_has_error("a :: () {} a(0)");
    assert_has_error("a :: (ar1) {} a()");
    assert_has_error("a :: (ar1) {} a(0, 0)");
    assert_has_error("a :: (ar1, ar2) {} a()");
    assert_has_error("a :: (ar1, ar2) {} a(0)");
    assert_has_error("a :: (ar1, ar2) {} a(0, 0, 0)");
}
