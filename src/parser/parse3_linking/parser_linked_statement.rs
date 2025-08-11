use crate::error::CompilationError as CE;
use crate::parser::operations::{OneSidedOperation, TwoSidedOperation};
use crate::parser::parse2_syntactic::statement::*;
use super::linked_statement::*;
use super::object::{ObjectFactory, ObjType};
use super::context_window::ObjectContextWindow;

pub fn link_names<'text>(statement: &Vec<Statement<'text>>, object_factory: &mut ObjectFactory) -> Result<Vec<LinkedStatement<'text>>, CE> {
    let mut context = LinkingContext::new(object_factory);
    context.link_statements_recursive(statement)
}

struct LinkingContext<'text, 'factory> {
    object_context_window: ObjectContextWindow<'text>,
    object_factory: &'factory mut ObjectFactory,
    current_function_returns: Option<ObjType>,
}
impl<'factory> LinkingContext<'_, 'factory> {
    fn new(object_factory: &'factory mut ObjectFactory) -> Self {
        Self {
            object_context_window: ObjectContextWindow::new(),
            object_factory,
            current_function_returns: None,
        }
    }
}

impl<'text> LinkingContext<'text, '_> {
    fn link_statements_recursive(&mut self, statements: &[Statement<'text>]) -> Result<Vec<LinkedStatement<'text>>, CE> {
        let mut result = vec![];
        for statement in statements {
            let linked = match statement {
                Statement::VariableDeclaration { object: name, typee, value } => {
                    let typed_expr = self.parse_expression(value)?;
                    if let Some(typee) = typee {
                        let typee = self.parse_type(typee)?;
                        if typee != typed_expr.typee {
                            return Err(CE::IncorrectType { got: typed_expr.typee, expected: typee })
                        }
                    }

                    let object = self.object_factory.create_object(name, typed_expr.typee.clone(), &mut self.object_context_window);
                    LinkedStatement::new_variable(object, typed_expr)
                }
                Statement::SetVariable { object: name, value } => {
                    let value = self.parse_expression(value)?;
                    let object = self.object_context_window.get_or_error(name)?;
                    LinkedStatement::new_set(object, value)
                }
                Statement::EqualSetVariable { object: name, value, op } => {
                    let value = self.parse_expression(value)?;
                    let object = self.object_context_window.get_or_error(name)?;

                    let object_type = self.object_factory.get_type(object);
                    let Some(result_type) = ObjType::from_operation(&value.typee, object_type, op) else {
                        return Err(CE::IncorrectTwoOper { type1: value.typee, type2: object_type.clone(), op: *op })
                    };
                    assert_eq!(&result_type, object_type, "EqualSet should not change type");

                    let object_value = TypedExpression::new(object_type.clone(), LinkedExpression::Variable(object));
                    let result_expression = TypedExpression::new(
                        result_type,
                        LinkedExpression::new_operation(object_value, value, *op)
                    );
                    LinkedStatement::new_set(object, result_expression)
                }
                Statement::Expression(expression) => {
                    let expression = self.parse_expression(expression)?;
                    LinkedStatement::Expression(expression)
                }
                Statement::If { condition, body } => {
                    let condition = self.parse_expression(condition)?;
                    check_condition_type(&condition)?;
                    self.object_context_window.step_in();
                    let body = self.link_statements_recursive(body)?;
                    self.object_context_window.step_out();
                    LinkedStatement::new_if(condition, body)
                }
                Statement::While { condition, body } => {
                    let condition = self.parse_expression(condition)?;
                    check_condition_type(&condition)?;
                    self.object_context_window.step_in();
                    let body = self.link_statements_recursive(body)?;
                    self.object_context_window.step_out();
                    LinkedStatement::new_while(condition, body)
                }
                Statement::Function { object: name, args, returns, body } => {
                    let mut arguments_obj = Vec::with_capacity(args.len());
                    let mut arguments_type = Vec::with_capacity(args.len());

                    for (name, typee) in args {
                        let typee = self.parse_type(typee)?;
                        arguments_type.push(typee.clone());
                        let object = self.object_factory.create_object(name, typee, &mut self.object_context_window);
                        arguments_obj.push(object);
                    }

                    let return_type = {
                        match returns {
                            Some(typee) => self.parse_type(typee)?,
                            None => ObjType::Unit,
                        }
                    };

                    let func_type = ObjType::Function {
                        arguments: arguments_type,
                        returns: Box::new(return_type.clone()),
                    };
                    if self.object_context_window.get(name).is_some() {
                        return Err(CE::FunctionOverloading { function_name: name.iter().collect::<String>()})
                    }
                    let function_object = self.object_factory.create_object(name, func_type, &mut self.object_context_window);

                    self.object_context_window.step_in();
                    self.current_function_returns = Some(return_type.clone());
                    let body = self.link_statements_recursive(body)?;
                    self.current_function_returns = None;
                    self.object_context_window.step_out();

                    if return_type != ObjType::Unit && !check_is_returns(&body) {
                        return Err(CE::FunctionMustReturn { function_name: name.iter().collect::<String>() })
                    }

                    LinkedStatement::new_function(function_object, arguments_obj, return_type, body)
                }
                Statement::Return(option_expression) => {
                    let expression = match option_expression {
                        Some(expression) => Some(self.parse_expression(expression)?),
                        None => None,
                    };
                    let expression_type = match &expression {
                        Some(expr) => &expr.typee,
                        None => &ObjType::Unit,
                    };
                    if Some(expression_type) != self.current_function_returns.as_ref() {
                        return match &self.current_function_returns {
                            None => Err(CE::UnexpectedReturn),
                            Some(cfr) => Err(CE::IncorrectType { got: expression_type.clone(), expected: cfr.clone() })
                        }
                    }
                    LinkedStatement::Return(expression)
                }
            };
            result.push(linked);
        }
        Ok(result)
    }

    fn parse_expression(&mut self, expression: &Expression<'text>) -> Result<TypedExpression<'text>, CE> {
        let linked: TypedExpression = match expression {
            Expression::Operation(expression1, expression2, op) => {
                let ex1 = self.parse_expression(expression1)?;
                let ex2 = self.parse_expression(expression2)?;
                let Some(result_type) = ObjType::from_operation(&ex1.typee, &ex2.typee, op) else {
                    return Err(CE::IncorrectTwoOper { type1: ex1.typee, type2: ex2.typee, op: *op })
                };
                TypedExpression::new(
                    result_type,
                    LinkedExpression::new_operation(ex1, ex2, *op)
                )
            }
            Expression::UnaryOperation(expression, op) => {
                let ex = self.parse_expression(expression)?;
                let Some(result_type) = ObjType::from_unary_operation(&ex.typee, op) else {
                    return Err(CE::IncorrectOneOper { typee: ex.typee, op: *op })
                };
                TypedExpression::new(
                    result_type,
                    LinkedExpression::new_unary_operation(ex, *op)
                )
            }
            Expression::NumberLiteral(string) => {
                TypedExpression::new(
                    ObjType::Number,
                    LinkedExpression::NumberLiteral(string),
                )
            },
            Expression::RoundBracket(ex1) => {
                let ex1 = self.parse_expression(ex1)?;
                TypedExpression::new(ex1.typee.clone(), LinkedExpression::new_round_bracket(ex1))
            }
            Expression::Variable(name) => {
                let object = self.object_context_window.get_or_error(name)?;
                if matches!(self.object_factory.get_type(object), ObjType::Function { .. }) {
                    return Err(CE::LinkingErrorFunctionUsage { name: CE::string_from(name) });
                }
                TypedExpression::new(
                    self.object_factory.get_type(object).clone(),
                    LinkedExpression::Variable(object)
                )
            },
            Expression::FunctionCall { object: name, args: args_values } => {
                let object = self.object_context_window.get_or_error(name)?;
                let ObjType::Function { arguments, returns } = self.object_factory.get_type(object).clone() else {
                    let context = format!("{:?}", self.object_context_window);
                    return Err(CE::LinkingError { name: CE::string_from(name), context });
                };
                if args_values.len() != arguments.len() {
                    let function_name = object.name.iter().collect::<String>();
                    return Err(CE::IncorrectArgumentCount { function_name, argument_need: arguments.len(), argument_got: args_values.len() });
                }
                let args = args_values.iter().map(|x| self.parse_expression(x)).collect::<Result<Vec<_>, _>>()?;

                TypedExpression::new(
                    returns.as_ref().clone(),
                    LinkedExpression::new_function_call(object, args)
                )
            }
        };
        Ok(linked)
    }
    
    fn parse_type(&self, typee: &Typee<'text>) -> Result<ObjType, CE> {
        match typee {
            Typee::String(chars) => {
                let string = chars.iter().collect::<String>();
                match string.as_str() {
                    "()" => Ok(ObjType::Unit),
                    "i32" => Ok(ObjType::Number),
                    "bool" => Ok(ObjType::Bool),
                    _ => Err(CE::LinkingError { name: string, context: format!("{:?}", self.object_context_window) }),
                }
            }
        }
    }
}

fn check_is_returns(statements: &[LinkedStatement]) -> bool {
    for statement in statements {
        match statement {
            LinkedStatement::Return(_) => return true,
            LinkedStatement::If { condition: _, body } => {
                if check_is_returns(body) {
                    return true;
                }
            }
            LinkedStatement::While { condition: _, body } => {
                if check_is_returns(body) {
                    return true;
                }
            }
            _ => ()
        }
    }
    false
}

impl ObjType {
    fn from_unary_operation(typee: &ObjType, one_sided_operation: &OneSidedOperation) -> Option<ObjType> {
        match one_sided_operation {
            OneSidedOperation::BoolNot => {
                if typee == &ObjType::Bool {
                    Some(ObjType::Bool)
                } else {
                    None
                }
            }
            OneSidedOperation::UnaryMinus => {
                if typee == &ObjType::Number {
                    Some(ObjType::Number)
                } else {
                    None
                }
            }
        }
    }
    fn from_operation(type1: &ObjType, type2: &ObjType, two_sided_operation: &TwoSidedOperation) -> Option<ObjType> {
        match two_sided_operation {
            TwoSidedOperation::Number(_) => {
                if type1 == &ObjType::Number && type2 == &ObjType::Number {
                    Some(ObjType::Number)
                } else {
                    None
                }
            }
            TwoSidedOperation::Bool(_) => {
                if type1 == &ObjType::Bool && type2 == &ObjType::Bool {
                    Some(ObjType::Bool)
                } else {
                    None
                }
            }
            TwoSidedOperation::Compare(comp_op) => {
                if type1 != type2 {
                    None
                } else if comp_op.is_equal_op() {
                    Some(ObjType::Bool)
                } else {
                    // FIXME: not all types can be compared
                    Some(ObjType::Bool)
                }
            }
        }
    }
}

fn check_condition_type(condition: &TypedExpression) -> Result<(), CE> {
    match condition.typee {
        ObjType::Bool => Ok(()),
        _ => Err(CE::IncorrectType { got: condition.typee.clone(), expected: ObjType::Bool }),
    }
}
