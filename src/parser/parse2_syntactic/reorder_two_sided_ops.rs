use crate::parser::parse2_syntactic::statement::{Statement, Expression};
use crate::parser::operations::TwoSidedOperation;

pub fn reorder_statements(statements: Vec<Statement>) -> Vec<Statement> {
    statements.into_iter().map(|statement| {
        match statement {
            Statement::VariableDeclaration { object, typee, value } => {
                Statement::VariableDeclaration { object, typee, value: reorder_expression(value) }
            }
            Statement::SetVariable { object, value } => {
                Statement::SetVariable { object, value: reorder_expression(value) }
            }
            Statement::EqualSetVariable { object, value, op } => {
                Statement::EqualSetVariable { object, value: reorder_expression(value), op }
            }
            Statement::Expression(expression) => {
                Statement::Expression(reorder_expression(expression))
            }
            Statement::If { condition, body } => {
                Statement::If { condition: reorder_expression(condition), body: reorder_statements(body) }
            }
            Statement::While { condition, body } => {
                Statement::While { condition: reorder_expression(condition), body: reorder_statements(body) }
            }
            Statement::Function { object, args, returns, body } => {
                Statement::Function { object, args, returns, body: reorder_statements(body) }
            }
            Statement::Return(option_expression) => {
                let expression = option_expression.map(reorder_expression);
                Statement::Return(expression)
            }
        }
    }).collect()
}

fn reorder_expression(expression: Expression) -> Expression {
    match expression {
        Expression::Operation(expression1, expression2, op) => {
            let expr = Expression::Operation(expression1, expression2, op);
            reorder_two_sided_expr(expr)
        }
        Expression::UnaryOperation(expression, op) => {
            Expression::new_unary_operation(reorder_expression(*expression), op)
        },
        Expression::NumberLiteral(string) => Expression::NumberLiteral(string),
        Expression::RoundBracket(ex1) => {
            Expression::new_round_bracket(reorder_expression(*ex1))
        }
        Expression::Variable(name) => Expression::Variable(name),
        Expression::FunctionCall { object, args } => {
            let args: Vec<Expression> = args.into_iter().map(|a| reorder_expression(a)).collect();
            Expression::FunctionCall { object, args }
        }
    }
}

fn reorder_two_sided_expr(exp: Expression) -> Expression {
    fn unwrap_expr<'a>(exp: Expression<'a>, exps: &mut Vec<Expression<'a>>, ops: &mut Vec<TwoSidedOperation>) {
        if let Expression::Operation(left, right, op) = exp {
            unwrap_expr(*left, exps, ops);
            ops.push(op);
            unwrap_expr(*right, exps, ops);
        } else {
            exps.push(exp);
        }
    }
    
    let mut exps: Vec<Expression> = Vec::new();
    let mut ops: Vec<TwoSidedOperation> = Vec::new();
    unwrap_expr(exp, &mut exps, &mut ops);
    
    fn create_expression(mut exps: Vec<Expression>, mut ops: Vec<TwoSidedOperation>) -> Expression {
        if ops.is_empty() {
            return exps.pop().unwrap()
        }
        let split_index = (0..ops.len()).min_by_key(|x| ops[*x].get_prior()).unwrap();
        
        let exps_right = exps.split_off(split_index + 1);
        let ops_right = ops.split_off(split_index + 1);
        let op_middle = ops.pop().unwrap();
        
        let left_exp = create_expression(exps, ops);
        let right_exp = create_expression(exps_right, ops_right);
        Expression::new_operation(left_exp, right_exp, op_middle)
    }
    
    create_expression(exps, ops)
}
