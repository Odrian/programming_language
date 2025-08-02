use crate::error::CompilationError as CE;

use super::super::parse3_syntactic::statement::*;
use super::linked_statement::*;
use super::context_window::ObjectContextWindow;

pub fn link_names<'text>(statement: &Vec<Statement<'text>>) -> Result<Vec<LinkedStatement<'text>>, CE> {
    let mut object_context_window = ObjectContextWindow::new();

    object_context_window.step_in();
    let result = link_statements_recursive(statement, &mut object_context_window);
    object_context_window.step_out();
    result
}

fn link_statements_recursive<'text>(statements: &[Statement<'text>], object_context_window: &mut ObjectContextWindow<'text>) -> Result<Vec<LinkedStatement<'text>>, CE> {
    let mut result = vec![];
    for statement in statements {
        let linked = match statement {
            Statement::VariableDeclaration { object: name, value } => {
                let value = parse_expression(value, object_context_window)?;
                let object = object_context_window.add(name, ObjType::Variable);
                LinkedStatement::new_variable(object, value)
            }
            Statement::SetVariable { object: name, value } => {
                let value = parse_expression(value, object_context_window)?;
                let name: String = name.iter().collect();
                let Some(&object) = object_context_window.get(&name) else {
                    let context = format!("{object_context_window:?}");
                    return Err(CE::LinkingError { name, context });
                };
                LinkedStatement::new_set(object, value)
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
                LinkedStatement::new_if(condition, body)
            }
            Statement::While { condition, body } => {
                let condition = parse_expression(condition, object_context_window)?;
                object_context_window.step_in();
                let body = link_statements_recursive(body, object_context_window)?;
                object_context_window.step_out();
                LinkedStatement::new_while(condition, body)
            }
            Statement::Function { object: name, args, body } => {
                let object = object_context_window.add(name, ObjType::Function);
                object_context_window.step_in();
                let args: Vec<Object> = args.iter().map(|x|
                    object_context_window.add(x, ObjType::Variable)
                ).collect();
                let body = link_statements_recursive(body, object_context_window)?;
                object_context_window.step_out();
                LinkedStatement::new_function(object, args, body)
            }
        };
        result.push(linked);
    }
    Ok(result)
}

fn parse_expression<'text>(expression: &Expression<'text>, object_context_window: &ObjectContextWindow<'text>) -> Result<LinkedExpression<'text>, CE> {
    let linked = match expression {
        Expression::Plus(expression1, expression2) => {
            let ex1 = parse_expression(expression1, object_context_window)?;
            let ex2 = parse_expression(expression2, object_context_window)?;
            LinkedExpression::new_plus(ex1, ex2)
        }
        Expression::NumberLiteral(string) => LinkedExpression::NumberLiteral(string),
        Expression::RoundBracket(ex1) => {
            let ex1 = parse_expression(ex1, object_context_window)?;
            LinkedExpression::new_round_bracket(ex1)
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
        Expression::FunctionCall { object: name, args } => {
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

            LinkedExpression::new_function_call(*object, args)
        }
    };
    Ok(linked)
}
