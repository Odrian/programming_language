use crate::error::CompilationError as CE;

use super::super::parse3_syntactic::statement::*;
use super::linked_statement::*;
use super::context_window::ObjectContextWindow;

pub fn link_variables(statement: Vec<Statement>) -> Result<Vec<LinkedStatement>, CE> {
    let mut object_context_window = ObjectContextWindow::new();

    object_context_window.step_in();
    let result = link_statements_recursive(&statement, &mut object_context_window);
    object_context_window.step_out();
    result
}


fn link_statements_recursive<'x>(statements: &[Statement<'x>], object_context_window: &mut ObjectContextWindow<'x>) -> Result<Vec<LinkedStatement<'x>>, CE> {
    let mut result = vec![];
    for statement in statements {
        let linked = match statement {
            Statement::VariableDeclaration { name, value } => {
                let value = parse_expression(value, object_context_window)?;
                let object = object_context_window.add(name, ObjType::Variable);
                LinkedStatement::VariableDeclaration { object, value }
            }
            Statement::SetVariable { name, value } => {
                let value = parse_expression(value, object_context_window)?;
                let name: String = name.iter().collect();
                let Some(&object) = object_context_window.get(&name) else {
                    let context = format!("{object_context_window:?}");
                    return Err(CE::LinkingError { name, context });
                };
                LinkedStatement::SetVariable { object, value }
            }
            Statement::Expression(expression) => {
                let expression = parse_expression(expression, object_context_window)?;
                LinkedStatement::Expression(expression)
            }
            Statement::If { condition, body } => {
                let condition = parse_expression(condition, object_context_window)?;
                object_context_window.step_in();
                let body = link_statements_recursive(body, object_context_window)?;
                object_context_window.step_out();
                LinkedStatement::If { condition, body }
            }
            Statement::While { condition, body } => {
                let condition = parse_expression(condition, object_context_window)?;
                object_context_window.step_in();
                let body = link_statements_recursive(body, object_context_window)?;
                object_context_window.step_out();
                LinkedStatement::While { condition, body }
            }
            Statement::Function { name, args, body } => {
                let object = object_context_window.add(name, ObjType::Function);
                object_context_window.step_in();
                let args: Vec<Object> = args.iter().map(|x|
                    object_context_window.add(x, ObjType::Variable)
                ).collect();
                let body = link_statements_recursive(body, object_context_window)?;
                object_context_window.step_out();
                LinkedStatement::Function { object, args, body }
            }
        };
        result.push(linked);
    }
    Ok(result)
}

fn parse_expression<'x>(expression: &Expression<'x>, object_context_window: &ObjectContextWindow<'x>) -> Result<LinkedExpression<'x>, CE> {
    let linked = match expression {
        Expression::Plus(expression1, expression2) => {
            let ex1 = parse_expression(expression1, object_context_window)?;
            let ex2 = parse_expression(expression2, object_context_window)?;
            LinkedExpression::Plus(Box::new(ex1), Box::new(ex2))
        }
        Expression::NumberLiteral(string) => LinkedExpression::NumberLiteral(string),
        Expression::RoundBracket(ex1) => {
            let ex1 = parse_expression(ex1, object_context_window)?;
            LinkedExpression::RoundBracket(Box::new(ex1))
        }
        Expression::Variable(name) => {
            let name: String = name.iter().collect();
            if let Some(object) = object_context_window.get(&name) {
                if object.obj_type == ObjType::Function {
                    return Err(CE::LinkingErrorFunctionUsage { name });
                }
                LinkedExpression::Variable(*object)
            } else {
                let context = format!("{object_context_window:?}");
                return Err(CE::LinkingError { name, context });
            }
        },
        Expression::FunctionCall { name, args } => {
            let name: String = name.iter().collect();
            let Some(object) = object_context_window.get(&name) else {
                let context = format!("{object_context_window:?}");
                return Err(CE::LinkingError { name, context });
            };
            if object.obj_type != ObjType::Function {
                let context = format!("{object_context_window:?}");
                return Err(CE::LinkingError { name, context });
            }
            let args = args.iter().map(|x| parse_expression(x, object_context_window)).collect::<Result<Vec<_>, _>>()?;

            LinkedExpression::FunctionCall { object: *object, args }
        }
    };
    Ok(linked)
}