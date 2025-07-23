use std::fmt::Display;
use crate::error::CompilationError as CE;
use crate::parser::parse2_brackets::{Token2WithPos, Token2, TwoSidedOperation, EqualOperation, BracketType};
use crate::parser::parse1_tokenize::PositionInFile;

pub fn parse_statements<'x>(tokens: &[Token2WithPos<'x>]) -> Result<Vec<Statement<'x>>, CE> {
    let mut statements = Vec::new();

    let mut index = 0;
    while index < tokens.len() {
        let (statement, new_index) = parse_statement(tokens, index)?;
        index = new_index;
        statements.push(statement);
    }

    Ok(statements)
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Statement<'x> {
    VariableDeclaration { name: &'x [char], value: Expression<'x> },
    SetVariable { name: &'x [char], value: Expression<'x> },
    Expression(Expression<'x>),
    // Bracket(Box<Vec<Statement>>, BracketType),
    If { condition: Expression<'x>, body: Vec<Statement<'x>> },
    While { condition: Expression<'x>, body: Vec<Statement<'x>> },
    Function { name: &'x [char], args: Vec<&'x [char]>, body: Vec<Statement<'x>> },
}

impl<'x> Statement<'x> {
    pub fn new_variable(name: &'x [char], value: Expression<'x>) -> Self {
        Statement::VariableDeclaration { name, value }
    }
    pub fn new_set(name: &'x [char], value: Expression<'x>) -> Self {
        Statement::SetVariable { name, value }
    }
    pub fn new_if(condition: Expression<'x>, body: Vec<Statement<'x>>) -> Self {
        Statement::If { condition, body }
    }
    pub fn new_while(condition: Expression<'x>, body: Vec<Statement<'x>>) -> Self {
        Statement::While { condition, body }
    }
    pub fn new_function(name: &'x [char], args: Vec<&'x [char]>, body: Vec<Statement<'x>>) -> Self {
        Statement::Function { name, args, body }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression<'x> {
    Plus(Box<Expression<'x>>, Box<Expression<'x>>),
    NumberLiteral(&'x [char]),
    Variable(&'x [char]),
    RoundBracket(Box<Expression<'x>>),
    FunctionCall { name: &'x [char], args: Vec<Expression<'x>> },
}
impl<'x> Expression<'x> {
    pub fn plus(expression1: Expression<'x>, expression2: Expression<'x>) -> Self {
        Expression::Plus(Box::new(expression1), Box::new(expression2))
    }
    pub fn round_bracket(expression: Expression<'x>) -> Self {
        Expression::RoundBracket(Box::new(expression))
    }
    pub fn new_function_call(name: &'x [char], args: Vec<Expression<'x>>) -> Self {
        Expression::FunctionCall { name, args }
    }
}

impl<'x> Display for Statement<'x> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn statements_to_string_with_tabs(statements: &[Statement]) -> String {
            let string = statements.iter().map(|s| s.to_string()).collect::<Vec<_>>().join("\n");
            "    ".to_string() + string.replace("\n", "\n    ").as_str()
        }
        match self {
            Self::VariableDeclaration { name, value } => {
                write!(f, "{} := {}", name.iter().collect::<String>(), value)
            }
            Self::SetVariable { name, value } => {
                write!(f, "{} = {}", name.iter().collect::<String>(), value)
            }
            Self::Expression(expression) => {
                write!(f, "{expression}")
            }
            Self::If { condition, body } => {
                let inside = statements_to_string_with_tabs(body);
                write!(f, "if {condition} {{\n{inside}\n}}")
            }
            Self::While { condition, body } => {
                let inside = statements_to_string_with_tabs(body);
                write!(f, "while {condition} {{\n{inside}\n}}")
            }
            Self::Function { name, args, body } => {
                let name = name.iter().collect::<String>();
                let args = args.iter().map(|s| s.iter().collect()).collect::<Vec<String>>().join(", ");
                let inside = statements_to_string_with_tabs(body);
                write!(f, "{name} :: ({args}) {{\n{inside}\n}}")
            }
            // Self::Bracket(statements, bracket) => {
            //     match bracket {
            //         BracketType::Curly => {
            //             write!(f, "{{\n{}\n}}", statements.iter().map(|s| s.to_string()).collect::<Vec<String>>().join("\n"))
            //         }
            //         BracketType::Round => {
            //             write!(f, "({})", statements.iter().map(|s| s.to_string()).collect::<Vec<String>>().join("\n"))
            //         }
            //         BracketType::None => panic!("should not be used")
            //     }
            // }
        }
    }
}
impl<'x> Display for Expression<'x> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Plus(a, b) => write!(f, "({a} + {b})"),
            Expression::NumberLiteral(n) => write!(f, "{}", n.iter().collect::<String>()),
            Expression::Variable(name) => write!(f, "{}", name.iter().collect::<String>()),
            Expression::RoundBracket(expression) => write!(f, "({expression})"),
            Expression::FunctionCall { name, args } => {
                let name = name.iter().collect::<String>();
                let args = args.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", ");
                write!(f, "{name} ({args})")
            }
        }
    }
}

fn parse_statement<'x>(tokens: &[Token2WithPos<'x>], index: usize) -> Result<(Statement<'x>, usize), CE> {
    if index >= tokens.len() {
        panic!("tokens should not be empty");
    }
    let token = &tokens[index];
    match &token.token {
        Token2::NumberLiteral(_) | Token2::TwoSidedOperation(_) | Token2::EqualOperation(_) | Token2::Comma | Token2::Colon | Token2::DoubleColon => {
            Err(CE::SyntacticsError(token.position, String::from("expected statement")))
        }
        Token2::String(chars) => {
            let string = chars.iter().collect::<String>();
            match string.as_str() {
                "if" | "while" => {
                    let (condition, new_index) = parse_expression(tokens, index + 1, token.position)?;
                    if new_index >= tokens.len() {
                        return Err(CE::SyntacticsError(tokens.last().unwrap().position, format!("expected {string} body after that")));
                    }
                    let next_token = &tokens[new_index];
                    let Token2::Bracket(vec, BracketType::Curly) = &next_token.token else {
                        return Err(CE::SyntacticsError(token.position, format!("expected {string} body")))
                    };
                    let body = parse_statements(vec)?;

                    if string.as_str() == "if" {
                        Ok((Statement::new_if(condition, body), new_index + 1))
                    } else {
                        Ok((Statement::new_while(condition, body), new_index + 1))
                    }
                }
                _ => {
                    parse_statement2(tokens, index + 1, chars, token.position)
                }
            }
        }
        Token2::Bracket(_, _) => {
            Err(CE::SyntacticsError(token.position, format!("unexpected bracket open at {}, expected statement", token.position)))
        }
    }
}
fn parse_statement2<'x>(tokens: &[Token2WithPos<'x>], index: usize, string: &'x [char], previous_place_info: PositionInFile) -> Result<(Statement<'x>, usize), CE> {
    if tokens.is_empty() {
        return Err(CE::SyntacticsError(previous_place_info, String::from("expected statement")));
    }
    let new_token = &tokens[index].token;
    match new_token {
        Token2::DoubleColon => {
            parse_function(tokens, index+1, string, tokens[index].position)
        },
        Token2::EqualOperation(equal_operation) => {
            let (expression2, new_tokens) = parse_expression(tokens, index+1, tokens[index].position)?;
            let statement = match equal_operation {
                EqualOperation::ColonEqual => Statement::new_variable(string, expression2),
                EqualOperation::Equal => Statement::new_set(string, expression2),
            };
            Ok((statement, new_tokens))
        }
        Token2::Bracket(vec, BracketType::Round) => {
            let name = string;
            let args = parse_function_arguments(vec, tokens[index].position)?;
            let statement = Statement::Expression(Expression::FunctionCall { name, args });
            Ok((statement, index + 1))
        }
        _ => {
            let string = string.iter().collect::<String>();
            Err(CE::SyntacticsError(tokens[index].position, format!("expected statement, got '{string}' '{new_token:?}'")))
        }
    }
}

fn parse_expression<'x>(tokens: &[Token2WithPos<'x>], index: usize, previous_place_info: PositionInFile) -> Result<(Expression<'x>, usize), CE> {
    if index >= tokens.len() {
        return Err(CE::SyntacticsError(previous_place_info, String::from("expected expression after that")));
    }
    let token = &tokens[index];
    match &token.token {
        Token2::TwoSidedOperation(_) | Token2::EqualOperation(_) | Token2::Comma | Token2::Colon | Token2::DoubleColon => {
            Err(CE::SyntacticsError(token.position, String::from("expected expression")))
        }
        Token2::String(string) => {
            let expression1 = Expression::Variable(string);
            parse_expression2(tokens, index+1, expression1)
        }
        Token2::NumberLiteral(value) => {
            let expression1 = Expression::NumberLiteral(value);
            parse_expression2(tokens, index+1, expression1)
        }
        Token2::Bracket(vec, BracketType::Round) => {
            let (expression, new_index) = parse_expression(vec, 0, token.position)?;
            if new_index >= vec.len() {
                let expression1 = Expression::round_bracket(expression);
                parse_expression2(tokens, index + 1, expression1)
            } else {
                Err(CE::SyntacticsError(vec[new_index].position, String::from("expected ')'")))
            }
        }
        Token2::Bracket(_, _) => {
            Err(CE::SyntacticsError(token.position, String::from("expected expression, got open bracket")))
        }
    }
}
// parse "expression1 twoSidedOp expression2"
fn parse_expression2<'x>(tokens: &[Token2WithPos<'x>], index: usize, expression1: Expression<'x>) -> Result<(Expression<'x>, usize), CE> {
    if index >= tokens.len() {
        return Ok((expression1, index));
    }
    let token = &tokens[index];
    match &token.token {
        Token2::String(_) | Token2::NumberLiteral(_) | Token2::EqualOperation(_) | Token2::Comma | Token2::Colon | Token2::DoubleColon => {
            Ok((expression1, index))
        }
        Token2::TwoSidedOperation(op) => {
            match op {
                TwoSidedOperation::Plus => {
                    let (expression2, new_tokens) = parse_expression(tokens, index+1, token.position)?;
                    let expression = Expression::plus(expression1, expression2);
                    Ok((expression, new_tokens))
                }
            }
        }
        Token2::Bracket(vec, BracketType::Round) => {
            let Expression::Variable(name) = expression1 else {
                return Err(CE::SyntacticsError(tokens[index].position, String::from("unexpected round brackets after expression")));
            };
            let args = parse_function_arguments(vec, token.position)?;
            let expression = Expression::FunctionCall { name, args };
            Ok((expression, index + 1))
        }
        Token2::Bracket(_, _) => {
            Ok((expression1, index))
        }
    }
}

fn parse_function<'x>(tokens: &[Token2WithPos<'x>], index: usize, name: &'x [char], previous_place_info: PositionInFile) -> Result<(Statement<'x>, usize), CE> {
    if index >= tokens.len() {
        return Err(CE::SyntacticsError(previous_place_info, String::from("expected function declaration")));
    }
    let Token2::Bracket(args, BracketType::Round) = &tokens[index].token else {
        return Err(CE::SyntacticsError(tokens[index].position, String::from("expected function declaration")));
    };

    // parse arguments
    let arguments: Vec<&[char]> = if args.is_empty() {
        Vec::new()
    } else {
        let mut arguments = Vec::with_capacity(args.len().div_ceil(2));

        let Token2::String(arg1) = &args[0].token else {
            return Err(CE::SyntacticsError(args[0].position, String::from("expected argument name")));
        };
        arguments.push(*arg1);

        let mut index = 1;
        while index < args.len() {
            if args[index].token != Token2::Comma {
                return Err(CE::SyntacticsError(args[index].position, String::from("expected ',' or ')'")));
            }
            index += 1;
            if index == args.len() {
                // TODO: break
                return Err(CE::SyntacticsError(args[index-1].position, String::from("expected argument after comma")));
            }
            let Token2::String(arg_i) = &args[index].token else {
                return Err(CE::SyntacticsError(args[index].position, String::from("expected argument name in function declaration")));
            };
            arguments.push(*arg_i);
            index += 1;
        }

        arguments
    };

    if index + 1 >= tokens.len() {
        return Err(CE::SyntacticsError(tokens[index].position, String::from("expected curly brackets after function declaration")));
    }

    // parse inside
    let Token2::Bracket(body, BracketType::Curly) = &tokens[index + 1].token else {
        return Err(CE::SyntacticsError(tokens[index + 1].position, String::from("expected curly brackets after function declaration")));
    };
    let body = parse_statements(body)?;

    let statement = Statement::new_function(name, arguments, body);
    Ok((statement, index + 2))
}

fn parse_function_arguments<'x>(tokens: &[Token2WithPos<'x>], previous_place_info: PositionInFile) -> Result<Vec<Expression<'x>>, CE> {
    let mut args = Vec::new();

    let mut index = 0;
    while index < tokens.len() {
        let (expression, new_index) = parse_expression(tokens, index, previous_place_info)?;
        args.push(expression);

        if new_index == tokens.len() {
            index = new_index;
        } else if tokens[new_index].token == Token2::Comma {
            index = new_index + 1;
        } else {
            return Err(CE::SyntacticsError(tokens[new_index].position, format!("expected ',' or ')', got {:?}", tokens[new_index].token)));
        }
    }

    Ok(args)
}

#[cfg(test)]
mod tests {
    use super::*;
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
            Ok(vec![Statement::Expression(Expression::FunctionCall { name: v_foo, args: vec![] })])
        );
        assert_result("foo(cat, 5)",
            Ok(vec![Statement::Expression(Expression::FunctionCall { name: v_foo, args: vec![
                Expression::Variable(v_cat),
                Expression::NumberLiteral(v_5),
            ] })])
        );
    }
}
