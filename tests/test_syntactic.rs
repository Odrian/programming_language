use programming_language::error::CompilationError as CE;
use programming_language::parser::*;
use programming_language::parser::parse1_tokenize::token::TwoSidedOperation;
use programming_language::parser::parse2_syntactic::statement::*;

fn parse(text: &[char]) -> Result<Vec<Statement>, CE> {
    let tokens = parse1_tokenize::tokenize(text)?;
    let statements = parse2_syntactic::parse_statements(&tokens)?;
    Ok(statements)
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
fn assert_result(str: &str, result: Result<Vec<Statement>, CE>) {
    assert_eq!(parse(&string_to_chars(str)), result);
}

fn add_type<'a, 'b>(typee: Typee<'b>, args: Vec<&'a[char]>) -> Vec<(&'a[char], Typee<'b>)> {
    args.iter().map(|a| (*a, typee)).collect()
}

#[test]
fn test_set() {
    assert_no_error("cat = cat");
    assert_no_error("cat = 4");
    assert_no_error("cat = cat + cat");
    assert_no_error("cat = cat + 4");
    assert_no_error("cat = 4 + cat");
    assert_no_error("cat = 4 + 4");
    assert_no_error("cat = cat + cat + cat");
    assert_no_error("cat = cat + cat + cat + cat");
    assert_no_error("cat := cat + cat + cat + cat");
    assert_no_error("cat : i32 = cat");

    assert_has_error("4 = cat");
    assert_has_error("cat = cat = cat");
    assert_has_error("cat + cat = cat");
    assert_has_error("cat + 4 = cat");
    assert_has_error("cat + cat");
    assert_has_error("cat += cat");
    assert_has_error("cat =+ cat");
    assert_has_error("cat = +cat");
    assert_has_error("cat == cat");
    assert_has_error("cat := +cat");
    assert_has_error("cat :== cat");
    assert_has_error("cat : = cat")
}

#[test]
fn test_set_with_brackets() {
    assert_no_error("cat = (4 + 4)");
    assert_no_error("cat = ((4 + 4))");
    assert_no_error("cat = 4 + (4 + 4)");
    assert_no_error("cat = (4 + 4) + 4");
    assert_no_error("cat = cat \n cat = cat");

    assert_has_error("(cat + cat) = cat");
    assert_has_error("(cat + cat) := cat");
    assert_has_error("(4 + 4) = 4");
    assert_has_error("(4 + 4) := 4");

    let v_cat = &string_to_chars("cat");
    let variable = Expression::Variable(v_cat);
    assert_result(
        "cat := cat + (cat + cat)",
        Ok(vec![Statement::new_variable(
            v_cat,
            None,
            Expression::new_two_sided_op(
                variable.clone(),
                Expression::new_round_bracket(
                    Expression::new_two_sided_op(variable.clone(), variable.clone(), TwoSidedOperation::Plus)
                ),
                TwoSidedOperation::Plus
            )
        )])
    );
}

#[test]
fn test_minus() {
    assert_no_error("a := 0 - 0");
    assert_no_error("a := 0 - 0 a = 0 - a");
    assert_no_error("a := 0 - 0 a = a - 0");

    assert_has_error("a := -1");
    assert_has_error("a := --1");
    assert_has_error("a := 0 a = -a");
}

#[test]
fn test_if_while() {
    let v_cat = &string_to_chars("cat");
    let variable = Expression::Variable(v_cat);

    assert_result("if cat { }",
                  Ok(vec![Statement::new_if(variable.clone(), vec![], )])
    );
    assert_result("if cat { cat := cat }",
                  Ok(vec![Statement::new_if(variable.clone(),
                                            vec![Statement::new_variable(v_cat, None, variable.clone())],
                  )])
    );
    assert_result("if cat { cat := cat cat := cat }",
                  Ok(vec![Statement::new_if(variable.clone(),
                                            vec![
                                                Statement::new_variable(v_cat, None, variable.clone()),
                                                Statement::new_variable(v_cat, None, variable.clone())
                                            ],
                  )])
    );
    assert_result("while cat { }",
                  Ok(vec![Statement::new_while(variable.clone(), vec![], )])
    );
    assert_result("while cat { cat := cat }",
                  Ok(vec![Statement::new_while(variable.clone(),
                                               vec![Statement::new_variable(v_cat, None, variable.clone())],
                  )])
    );

    assert_has_error("if cat = cat { cat := cat }");
    assert_has_error("if { cat = cat }");
}

#[test]
fn test_function() {
    assert_no_error("foo :: () { }");
    assert_no_error("foo :: () -> i32 { }");

    assert_has_error(":: () { }");
    assert_has_error("foo () { }");
    assert_has_error("foo :: { }");
    assert_has_error("foo :: ()");
    assert_has_error("foo :: () -> i32");

    let name = &string_to_chars("foo");
    let arg1 = &string_to_chars("arg1");
    let arg2 = &string_to_chars("arg2");
    let v_cat = &string_to_chars("cat");
    let v_dog = &string_to_chars("dog");
    let v_kitten = &string_to_chars("kitten");
    let v_owl = &string_to_chars("owl");

    let number_string = string_to_chars("i32");
    let number_typee = Typee::String(&number_string);

    assert_result(
        "foo :: () { }",
        Ok(vec![Statement::new_function(name, vec![], None, vec![])])
    );
    assert_result(
        "foo :: (arg1: i32) { }",
        Ok(vec![Statement::new_function(name, add_type(number_typee, vec![arg1]), None, vec![])])
    );
    assert_result(
        "foo :: (arg1: i32, arg2: i32) { }",
        Ok(vec![Statement::new_function(name, add_type(number_typee, vec![arg1, arg2]), None, vec![])])
    );
    let set_expr = Statement::new_variable(v_cat, None, Expression::Variable(v_dog));
    assert_result(
        "foo :: (arg1: i32, arg2: i32) { cat := dog }",
        Ok(vec![Statement::new_function(name, add_type(number_typee, vec![arg1, arg2]), None, vec![
            set_expr.clone(),
        ])])
    );

    assert_result(
        "foo :: (arg1: i32, arg2: i32) { cat := dog cat := dog }",
            Ok(vec![Statement::new_function(name, add_type(number_typee, vec![arg1, arg2]), None, vec![
                set_expr.clone(),
                set_expr.clone(),
            ])])
    );
    let another_set_expr = Statement::new_variable(v_owl, None, Expression::new_two_sided_op(
        Expression::Variable(v_dog), Expression::Variable(v_kitten), TwoSidedOperation::Plus
    ));
    assert_result(
        "foo :: (arg1: i32, arg2: i32) { owl := dog + kitten cat := dog owl := dog + kitten }",
        Ok(vec![Statement::new_function(name, add_type(number_typee, vec![arg1, arg2]), None, vec![
            another_set_expr.clone(),
            set_expr.clone(),
            another_set_expr.clone(),
        ])])
    );
}

#[test]
fn test_function_with_while() {
    let name = &string_to_chars("foo");
    let arg1 = &string_to_chars("arg1");
    let arg2 = &string_to_chars("arg2");
    let v_1 = &string_to_chars("1");

    let number_string = string_to_chars("i32");
    let number_typee = Typee::String(&number_string);

    let set_statement = Statement::new_variable(
        arg2,
        None,
        Expression::new_two_sided_op(
            Expression::Variable(arg2),
            Expression::NumberLiteral(v_1),
            TwoSidedOperation::Plus
        ),
    );
    let while_statement = Statement::new_while(
        Expression::Variable(arg1),
        vec![set_statement.clone()]
    );
    let create_function_statement = |body| {
        Statement::new_function(name, add_type(number_typee, vec![arg1, arg2]), None, body)
    };
    assert_result(
        "foo :: (arg1: i32, arg2: i32) { while arg1 { arg2 := arg2 + 1 } }",
        Ok(vec![create_function_statement(vec![
            while_statement.clone(),
        ])])
    );
    assert_result(
        "foo::(arg1:i32,arg2:i32){while arg1{arg2:=arg2+1}}",
        Ok(vec![create_function_statement(vec![
            while_statement.clone(),
        ])])
    );
    assert_result(
        "foo :: (arg1: i32, arg2: i32) { arg2 := arg2 + 1 while arg1 { arg2 := arg2 + 1 } }",
        Ok(vec![create_function_statement(vec![
            set_statement.clone(),
            while_statement.clone(),
        ])])
    );
    assert_result(
        "foo :: (arg1: i32, arg2: i32) { arg2 := arg2 + 1 while arg1 { arg2 := arg2 + 1 } arg2 := arg2 + 1 }",
        Ok(vec![create_function_statement(vec![
            set_statement.clone(),
            while_statement.clone(),
            set_statement.clone(),
        ])])
    );
}

#[test]
fn test_function_call() {
    assert_no_error("foo()");
    assert_no_error("cat = foo()");
    assert_no_error("foo(arg1)");
    assert_no_error("foo(arg1, arg2)");
    assert_no_error("foo(0, 1)");
    assert_no_error("foo((0 + 1))");
    assert_no_error("foo(foo(0))");

    assert_has_error("foo(cat := cat)");

    let v_foo = &string_to_chars("foo");
    let v_cat = &string_to_chars("cat");
    let v_5 = &string_to_chars("5");
    assert_result(
        "foo()",
        Ok(vec![Statement::Expression(Expression::new_function_call(v_foo, vec![]))])
    );
    assert_result(
        "foo(cat, 5)",
        Ok(vec![Statement::Expression(Expression::new_function_call(v_foo, vec![
                      Expression::Variable(v_cat),
                      Expression::NumberLiteral(v_5),
                  ]))])
    );
}

#[test]
fn test_function_return() {
    assert_no_error("return 0");
    assert_no_error("foo :: () { return 0 }");
    assert_no_error("foo :: () { a := 0 return a }");
    
    assert_has_error("return");
    assert_has_error("return return 0");
    assert_has_error("return foo :: () {}");
    assert_has_error("return ()");
}
