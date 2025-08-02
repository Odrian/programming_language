use crate::error::CompilationError as CE;

pub mod statement;

mod parser_statements;

use super::parse2_brackets::token2::Token2WithPos;
use statement::Statement;

pub fn parse_statements<'x>(tokens: &[Token2WithPos<'x>]) -> Result<Vec<Statement<'x>>, CE> {
    let mut statements = Vec::new();

    let mut state = parser_statements::ParsingState::new(tokens);
    while !state.at_end() {
        statements.push(state.parse_statement()?);
    }

    Ok(statements)
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::statement::*;
    use super::super::{parse1_tokenize, parse2_brackets};

    fn parse(text: &[char]) -> Result<Vec<Statement>, CE> {
        let tokens = parse1_tokenize::tokenize(text)?;
        let tokens2 = parse2_brackets::parse_brackets(tokens)?;
        let statements = parse_statements(&tokens2)?;
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
        assert_result("cat := cat + (cat + cat)",
            Ok(vec![Statement::new_variable(
                v_cat,
                Expression::plus(
                    variable.clone(),
                    Expression::round_bracket(
                        Expression::plus(variable.clone(), variable.clone())
                    )
                )
            )])
        );
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
                                      vec![Statement::new_variable(v_cat, variable.clone())],
            )])
        );
        assert_result("if cat { cat := cat cat := cat }",
           Ok(vec![Statement::new_if(variable.clone(),
                vec![
                    Statement::new_variable(v_cat, variable.clone()),
                    Statement::new_variable(v_cat, variable.clone())
                ],
           )])
        );
        assert_result("while cat { }",
           Ok(vec![Statement::new_while(variable.clone(), vec![], )])
        );
        assert_result("while cat { cat := cat }",
            Ok(vec![Statement::new_while(variable.clone(),
                                         vec![Statement::new_variable(v_cat, variable.clone())],
            )])
        );

        assert_has_error("if cat = cat { cat := cat }");
        assert_has_error("if { cat = cat }");
    }
    #[test]
    fn test_function() {
        assert_no_error("foo :: () { }");

        assert_has_error(":: () { }");
        assert_has_error("foo () { }");
        assert_has_error("foo :: { }");
        assert_has_error("foo :: ()");

        let name = &string_to_chars("foo");
        let arg1 = &string_to_chars("arg1");
        let arg2 = &string_to_chars("arg2");
        let v_cat = &string_to_chars("cat");
        let v_dog = &string_to_chars("dog");
        let v_kitten = &string_to_chars("kitten");
        let v_owl = &string_to_chars("owl");

        assert_result("foo :: () { }",
            Ok(vec![Statement::new_function(name, vec![], vec![])])
        );
        assert_result("foo :: (arg1) { }",
                   Ok(vec![Statement::new_function(name, vec![arg1], vec![])])
        );
        assert_result("foo :: (arg1, arg2) { }",
                   Ok(vec![Statement::new_function(name, vec![arg1, arg2], vec![])])
        );
        let set_expr = Statement::new_variable(v_cat, Expression::Variable(v_dog));
        assert_result("foo :: (arg1, arg2) { cat := dog }",
           Ok(vec![Statement::new_function(name, vec![arg1, arg2], vec![
               set_expr.clone(),
           ])])
        );

        assert_result("foo :: (arg1, arg2) { cat := dog cat := dog }",
           Ok(vec![Statement::new_function(name, vec![arg1, arg2], vec![
               set_expr.clone(),
               set_expr.clone(),
           ])])
        );
        let another_set_expr = Statement::new_variable(v_owl, Expression::plus(
            Expression::Variable(v_dog), Expression::Variable(v_kitten)
        ));
        assert_result("foo :: (arg1, arg2) { owl := dog + kitten cat := dog owl := dog + kitten }",
           Ok(vec![Statement::new_function(name, vec![arg1, arg2], vec![
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
        let set_statement = Statement::new_variable(
            arg2,
            Expression::plus(
                Expression::Variable(arg2),
                Expression::NumberLiteral(v_1),
            ),
        );
        let while_statement = Statement::new_while(
            Expression::Variable(arg1),
            vec![set_statement.clone()]
        );
        let create_function_statement = |body| {
            Statement::new_function(name, vec![arg1, arg2], body)
        };
        assert_result("foo :: (arg1, arg2) { while arg1 { arg2 := arg2 + 1 } }",
           Ok(vec![create_function_statement(vec![
               while_statement.clone(),
           ])])
        );
        assert_result("foo::(arg1,arg2){while arg1{arg2:=arg2+1}}",
           Ok(vec![create_function_statement(vec![
               while_statement.clone(),
           ])])
        );
        assert_result("foo :: (arg1, arg2) { arg2 := arg2 + 1 while arg1 { arg2 := arg2 + 1 } }",
           Ok(vec![create_function_statement(vec![
               set_statement.clone(),
               while_statement.clone(),
           ])])
        );
        assert_result("foo :: (arg1, arg2) { arg2 := arg2 + 1 while arg1 { arg2 := arg2 + 1 } arg2 := arg2 + 1 }",
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
        assert_result("foo()",
            Ok(vec![Statement::Expression(Expression::function_call(v_foo, vec![]))])
        );
        assert_result("foo(cat, 5)",
            Ok(vec![Statement::Expression(Expression::function_call(v_foo, vec![
                Expression::Variable(v_cat),
                Expression::NumberLiteral(v_5),
            ]))])
        );
    }
}
