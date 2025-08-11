use programming_language::error::CompilationError as CE;
use programming_language::parser::*;
use programming_language::parser::operations::*;
use programming_language::parser::parse2_syntactic::statement::*;

fn parse(text: &str) -> Result<Vec<Statement>, CE> {
    let tokens = parse1_tokenize::tokenize(text)?;
    let statements = parse2_syntactic::parse_statements(tokens)?;
    Ok(statements)
}
fn assert_has_error(str: &str) {
    assert_ne!(parse(str).err(), None);
}
fn assert_no_error(str: &str) {
    assert_eq!(parse(str).err(), None);
}
fn assert_result(str: &str, result: Result<Vec<Statement>, CE>) {
    assert_eq!(parse(str), result);
}

fn add_type(typee: Typee, args: Vec<String>) -> Vec<(String, Typee)> {
    args.into_iter().map(|a| (a, typee.clone())).collect()
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
    assert_no_error("cat = cat;cat = cat");
    assert_no_error("cat = cat; \n cat = cat");

    assert_has_error("(cat + cat) = cat");
    assert_has_error("(cat + cat) := cat");
    assert_has_error("(4 + 4) = 4");
    assert_has_error("(4 + 4) := 4");

    let variable = Expression::Variable("cat".to_owned());
    assert_result(
        "cat := cat + (cat + cat)",
        Ok(vec![Statement::new_variable(
            "cat".to_owned(),
            None,
            Expression::new_operation(
                variable.clone(),
                Expression::new_round_bracket(
                    Expression::new_operation(variable.clone(), variable.clone(), NumberOperation::Add.into())
                ),
                NumberOperation::Add.into()
            )
        )])
    );
}

#[test]
fn test_minus() {
    assert_no_error("a := 0 - 0");
    assert_no_error("a := 0 - 0; a = 0 - a");
    assert_no_error("a := 0 - 0; a = a - 0");

    assert_no_error("a := -1");
    assert_no_error("a := --1");
    assert_no_error("a := 0; a = -a");
}

#[test]
fn test_two_sided_ops() {
    assert_no_error("a := a * a / a + a - a % a");
    assert_no_error("a := (a * (a / (a + (a - (a % a)))))");
    
    // correct order is tested at compiling
}

#[test]
fn test_equal_set() {
    assert_no_error("a += a");
    assert_no_error("a -= a");
    assert_no_error("a *= a");
    assert_no_error("a /= a");
    assert_no_error("a %= a");
    // assert_no_error("a &= a");
    // assert_no_error("a &&= a");
    // assert_no_error("a |= a");
    // assert_no_error("a ||= a");
}

#[test]
fn test_if_while() {
    let variable = Expression::Variable("cat".to_owned());

    assert_result(
        "if cat { }",
        Ok(vec![Statement::new_if(variable.clone(), vec![], )])
    );
    assert_result(
        "if cat { cat := cat }",
        Ok(vec![Statement::new_if(variable.clone(), vec![
            Statement::new_variable("cat".to_owned(), None, variable.clone())
        ], )])
    );
    assert_result(
        "if cat { cat := cat; cat := cat }",
        Ok(vec![Statement::new_if(variable.clone(), vec![
            Statement::new_variable("cat".to_owned(), None, variable.clone()),
            Statement::new_variable("cat".to_owned(), None, variable.clone())
        ])])
    );
    assert_result(
        "while cat { }",
        Ok(vec![
            Statement::new_while(variable.clone(), vec![])
        ])
    );
    assert_result(
        "while cat { cat := cat }",
        Ok(vec![Statement::new_while(variable.clone(), vec![
            Statement::new_variable("cat".to_owned(), None, variable.clone())
        ])])
    );

    assert_has_error("if cat = cat { cat := cat }");
    assert_has_error("if { cat = cat }");
}

#[test]
fn test_operations() {
    assert_no_error("a := a & a");
    assert_no_error("a := a | a");
    assert_no_error("a := a & a | a");

    assert_no_error("if a != b {}");
    assert_no_error("while a != b {}");
    assert_no_error("while a == b {}");
    assert_no_error("while a > b {}");
    assert_no_error("while a < b {}");
    assert_no_error("while a >= b {}");
    assert_no_error("while a <= b {}");

    assert_no_error("if a && c {}");
    assert_no_error("if a || c {}");
    assert_no_error("if a || c && d {}");
    assert_no_error("if a && c || d {}");
    assert_no_error("if a == b && c == d {}");
    assert_no_error("if a == b || c == d {}");

    assert_no_error("if !a {}");
    assert_no_error("if -a {}");
}

#[test]
fn test_function() {
    assert_no_error("foo :: () { }");
    assert_no_error("foo :: () -> i32 { }");
    assert_no_error("foo :: () { return }");
    assert_no_error("foo :: () { return 0 }");

    assert_has_error(":: () { }");
    assert_has_error("foo () { }");
    assert_has_error("foo :: { }");
    assert_has_error("foo :: ()");
    assert_has_error("foo :: () -> i32");

    let name = "foo".to_owned();
    let arg1 = "arg1".to_owned();
    let arg2 = "arg2".to_owned();
    let v_cat = "cat".to_owned();
    let v_dog = "dog".to_owned();
    let v_kitten = "kitten".to_owned();
    let v_owl = "owl".to_owned();

    let number_typee = Typee::String("i32".to_owned());

    assert_result(
        "foo :: () { }",
        Ok(vec![Statement::new_function(name.clone(), vec![], None, vec![])])
    );
    assert_result(
        "foo :: (arg1: i32) { }",
        Ok(vec![Statement::new_function(name.clone(), add_type(number_typee.clone(), vec![arg1.clone()]), None, vec![])])
    );
    assert_result(
        "foo :: (arg1: i32, arg2: i32) { }",
        Ok(vec![Statement::new_function(name.clone(), add_type(number_typee.clone(), vec![arg1.clone(), arg2.clone()]), None, vec![])])
    );
    let set_expr = Statement::new_variable(v_cat, None, Expression::Variable(v_dog.clone()));
    assert_result(
        "foo :: (arg1: i32, arg2: i32) { cat := dog }",
        Ok(vec![Statement::new_function(name.clone(), add_type(number_typee.clone(), vec![arg1.clone(), arg2.clone()]), None, vec![
            set_expr.clone(),
        ])])
    );

    assert_result(
        "foo :: (arg1: i32, arg2: i32) { cat := dog; cat := dog }",
            Ok(vec![Statement::new_function(name.clone(), add_type(number_typee.clone(), vec![arg1.clone(), arg2.clone()]), None, vec![
                set_expr.clone(),
                set_expr.clone(),
            ])])
    );
    let another_set_expr = Statement::new_variable(v_owl.clone(), None, Expression::new_operation(
        Expression::Variable(v_dog.clone()), Expression::Variable(v_kitten), NumberOperation::Add.into()
    ));
    assert_result(
        "foo :: (arg1: i32, arg2: i32) { owl := dog + kitten; cat := dog; owl := dog + kitten }",
        Ok(vec![Statement::new_function(name.clone(), add_type(number_typee.clone(), vec![arg1.clone(), arg2.clone()]), None, vec![
            another_set_expr.clone(),
            set_expr.clone(),
            another_set_expr.clone(),
        ])])
    );
}

#[test]
fn test_function_with_while() {
    let name = "foo".to_owned();
    let arg1 = "arg1".to_owned();
    let arg2 = "arg2".to_owned();
    let v_1 = "1".to_owned();

    let number_typee = Typee::String("i32".to_owned());

    let set_statement = Statement::new_variable(
        arg2.clone(),
        None,
        Expression::new_operation(
            Expression::Variable(arg2.clone()),
            Expression::NumberLiteral(v_1).clone(),
            NumberOperation::Add.into()
        ),
    );
    let while_statement = Statement::new_while(
        Expression::Variable(arg1.clone()),
        vec![set_statement.clone()]
    );
    let create_function_statement = |body| {
        Statement::new_function(name.clone(), add_type(number_typee.clone(), vec![arg1.clone(), arg2.clone()]), None, body)
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
        "foo :: (arg1: i32, arg2: i32) { arg2 := arg2 + 1; while arg1 { arg2 := arg2 + 1 } }",
        Ok(vec![create_function_statement(vec![
            set_statement.clone(),
            while_statement.clone(),
        ])])
    );
    assert_result(
        "foo :: (arg1: i32, arg2: i32) { arg2 := arg2 + 1; while arg1 { arg2 := arg2 + 1 } arg2 := arg2 + 1 }",
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

    assert_no_error("a = foo(0) + foo(0) + foo(0) + 0");

    assert_no_error("0 + 0");
    assert_no_error("foo(0) + foo(0)");
    assert_no_error("foo(0) + foo(0) + foo(0) + 0");

    assert_has_error("foo(cat := cat)");

    let v_foo = "foo".to_owned();
    let v_cat = "cat".to_owned();
    let v_5 = "5".to_owned();
    assert_result(
        "foo()",
        Ok(vec![Statement::Expression(Expression::new_function_call(v_foo.clone(), vec![]))])
    );
    assert_result(
        "foo(cat, 5)",
        Ok(vec![Statement::Expression(Expression::new_function_call(v_foo.clone(), vec![
            Expression::Variable(v_cat),
            Expression::NumberLiteral(v_5),
        ]))])
    );
}

#[test]
fn test_function_return() {
    assert_no_error("return");
    assert_no_error("if 1 { return }");
    assert_no_error("return 0");
    assert_no_error("foo :: () { return 0 }");
    assert_no_error("foo :: () { a := 0; return a }");

    assert_has_error("return return 0");
    assert_has_error("return foo :: () {}");
    assert_has_error("return ()");
}
