use crate::error::CompilationError as CE;
use crate::parser::operations::{OneSidedOperation, TwoSidedOperation};
use crate::parser::parse2_syntactic::statement::*;
use super::linked_statement::*;
use super::object::{ObjectFactory, ObjType, FloatObjType, IntObjType};
use super::context_window::ObjectContextWindow;

pub fn link_names(statement: Vec<Statement>, object_factory: &mut ObjectFactory) -> Result<Vec<GlobalLinkedStatement>, CE> {
    let mut context = LinkingContext::new(object_factory);
    context.link_global_statements(statement)
}

struct LinkingContext<'factory> {
    object_context_window: ObjectContextWindow,
    object_factory: &'factory mut ObjectFactory,
    current_function_returns: Option<ObjType>,
}
impl<'factory> LinkingContext<'factory> {
    fn new(object_factory: &'factory mut ObjectFactory) -> Self {
        Self {
            object_context_window: ObjectContextWindow::new(),
            object_factory,
            current_function_returns: None,
        }
    }
}

impl LinkingContext<'_> {
    fn link_global_statements(&mut self, statements: Vec<Statement>) -> Result<Vec<GlobalLinkedStatement>, CE> {
        let mut result = vec![];

        for statement in statements {
            let global_statement = match statement {
                Statement::DeclarationStatement { name, statement } => self.parse_declaration_statement(name, statement)?,
                _ => return Err(CE::UnexpectedGlobalStatement { statement: statement.to_string() })
            };
            result.push(global_statement);
        }

        Ok(result)
    }
    fn link_statements_recursive(&mut self, statements: Vec<Statement>) -> Result<Vec<LinkedStatement>, CE> {
        let mut result = Vec::with_capacity(statements.len());
        for statement in statements {
            result.push(self.parse_statement(statement)?);
        }
        Ok(result)
    }
    fn parse_statement(&mut self, statement: Statement) -> Result<LinkedStatement, CE> {
        Ok(match statement {
            Statement::SetVariable { what, value, op } => {
                let what = self.parse_expression(what)?;
                let value = self.parse_expression(value)?;
                if what.object_type != value.object_type {
                    return Err(CE::IncorrectType { got: value.object_type, expected: what.object_type })
                }
                LinkedStatement::new_set(what, value, op)
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
            Statement::Return(option_expression) => {
                let expression = match option_expression {
                    Some(expression) => Some(self.parse_expression(expression)?),
                    None => None,
                };
                let expression_type = match &expression {
                    Some(expr) => &expr.object_type,
                    None => &ObjType::Void,
                };
                if Some(expression_type) != self.current_function_returns.as_ref() {
                    let expected_type = self.current_function_returns.clone().unwrap_or(ObjType::Void);
                    return Err(CE::IncorrectType { got: expression_type.clone(), expected: expected_type });
                }
                LinkedStatement::Return(expression)
            }
            Statement::DeclarationStatement { name, statement} => self.parse_declaration_statement(name, statement)?.into(),
            Statement::ComptimeStatement(statement) => match statement {
                ComptimeStatement::Import { .. } => unimplemented!(),
            }
        })
    }
    fn parse_declaration_statement(&mut self, name: String, declaration: DeclarationStatement) -> Result<GlobalLinkedStatement, CE> {
        Ok(match declaration {
            DeclarationStatement::VariableDeclaration { typee, value } => {
                let typed_expr = self.parse_expression(value)?;
                if let Some(typee) = typee {
                    let object_type = self.parse_type(typee)?;
                    if object_type != typed_expr.object_type {
                        return Err(CE::IncorrectType { got: typed_expr.object_type, expected: object_type })
                    }
                }
                if typed_expr.object_type.is_void() {
                    return Err(CE::UnexpectedVoidUse)
                }

                let object = self.object_factory.create_object(name, typed_expr.object_type.clone(), &mut self.object_context_window);
                GlobalLinkedStatement::new_variable(object, typed_expr)
            }
            DeclarationStatement::Function { args, returns, body } => {
                let mut arguments_obj = Vec::with_capacity(args.len());
                let mut arguments_type = Vec::with_capacity(args.len());

                for (arg_name, typee) in args {
                    let object_type = self.parse_type(typee)?;
                    if object_type.is_void() {
                        return Err(CE::UnexpectedVoidUse)
                    }
                    arguments_type.push(object_type.clone());
                    let object = self.object_factory.create_object(arg_name, object_type, &mut self.object_context_window);
                    arguments_obj.push(object);
                }

                let return_type = {
                    match returns {
                        Some(typee) => self.parse_type(typee)?,
                        None => ObjType::Void,
                    }
                };

                let func_type = ObjType::Function {
                    arguments: arguments_type,
                    returns: Box::new(return_type.clone()),
                };
                if self.object_context_window.get(&name).is_some() {
                    return Err(CE::FunctionOverloading { function_name: name })
                }

                self.object_context_window.step_in();
                self.current_function_returns = Some(return_type.clone());
                let mut body = self.link_statements_recursive(body)?;
                self.current_function_returns = None;
                self.object_context_window.step_out();

                if !check_is_returns(&body) {
                    if return_type == ObjType::Void {
                        body.push(LinkedStatement::Return(None));
                    } else {
                        return Err(CE::FunctionMustReturn { function_name: name })
                    }
                }

                let function_object = self.object_factory.create_object(name, func_type, &mut self.object_context_window);

                GlobalLinkedStatement::new_function(function_object, arguments_obj, return_type, body)
            }
            DeclarationStatement::Struct { .. } => unimplemented!(),
        })
    }
    fn parse_expression(&mut self, expression: Expression) -> Result<TypedExpression, CE> {
        let linked: TypedExpression = match expression {
            Expression::Operation(expression1, expression2, op) => {
                let ex1 = self.parse_expression(*expression1)?;
                let ex2 = self.parse_expression(*expression2)?;
                let Some(result_type) = ObjType::from_operation(&ex1.object_type, &ex2.object_type, op) else {
                    return Err(CE::IncorrectTwoOper { object_type1: ex1.object_type, object_type2: ex2.object_type, op })
                };
                TypedExpression::new(
                    result_type,
                    LinkedExpression::new_operation(ex1, ex2, op)
                )
            }
            Expression::UnaryOperation(expression, op) => {
                let ex = self.parse_expression(*expression)?;
                let Some(result_type) = ObjType::from_unary_operation(&ex.object_type, op) else {
                    return Err(CE::IncorrectOneOper { object_type: ex.object_type, op })
                };
                TypedExpression::new(
                    result_type,
                    LinkedExpression::new_unary_operation(ex, op)
                )
            }
            Expression::As(expression, typee) => {
                let ex = self.parse_expression(*expression)?;
                let object_type = self.parse_type(typee)?;
                
                let can_cast = ObjType::check_can_cast(&ex.object_type, &object_type);
                if !can_cast {
                    return Err(CE::IncorrectAs { what: ex.expr.to_string(), from: ex.object_type, to: object_type })
                }

                TypedExpression::new(
                    object_type.clone(),
                    LinkedExpression::new_as(ex, object_type),
                )
            }
            Expression::NumberLiteral(string) => {
                parse_number_literal(string)?
            },
            Expression::BoolLiteral(value) => {
                TypedExpression::new(
                    ObjType::BOOL,
                    LinkedExpression::BoolLiteral(value),
                )
            }
            Expression::CharLiteral(value) => {
                TypedExpression::new(
                    ObjType::Char,
                    LinkedExpression::CharLiteral(value),
                )
            }
            Expression::RoundBracket(ex1) => {
                let ex1 = self.parse_expression(*ex1)?;
                TypedExpression::new(ex1.object_type.clone(), LinkedExpression::new_round_bracket(ex1))
            }
            Expression::Variable(name) => {
                let object = self.object_context_window.get_or_error(&name)?;
                if matches!(self.object_factory.get_type(object), ObjType::Function { .. }) {
                    return Err(CE::LinkingErrorFunctionUsage { name });
                }
                TypedExpression::new(
                    self.object_factory.get_type(object).clone(),
                    LinkedExpression::Variable(object)
                )
            },
            Expression::FunctionCall { object: name, args: args_values } => {
                let object = self.object_context_window.get_or_error(&name)?;
                let ObjType::Function { arguments, returns } = self.object_factory.get_type(object).clone() else {
                    let context = format!("{:?}", self.object_context_window);
                    return Err(CE::LinkingError { name, context });
                };
                if args_values.len() != arguments.len() {
                    let function_name = self.object_factory.get_name(object).clone();
                    return Err(CE::IncorrectArgumentCount { function_name, argument_need: arguments.len(), argument_got: args_values.len() });
                }
                let args = args_values.into_iter().map(|x| self.parse_expression(x)).collect::<Result<Vec<_>, _>>()?;

                for index in 0..args.len() {
                    if args[index].object_type != arguments[index] {
                        return Err(CE::IncorrectType { got: args[index].object_type.clone(), expected: arguments[index].clone() })
                    }
                }

                TypedExpression::new(
                    returns.as_ref().clone(),
                    LinkedExpression::new_function_call(object, args)
                )
            }
        };
        Ok(linked)
    }

    fn parse_type(&self, typee: Typee) -> Result<ObjType, CE> {
        match typee {
            Typee::String(string) => {
                let primitive_option = parse_primitive_type(&string);
                if let Some(object_type) = primitive_option {
                    return Ok(object_type)
                }

                Err(CE::LinkingError { name: string, context: format!("{:?}", self.object_context_window) })
            }
            Typee::Reference(obj_type) => {
                let obj_type = self.parse_type(*obj_type)?;
                Ok(ObjType::Reference(Box::new(obj_type)))
            }
        }
    }
}

fn parse_primitive_type(string: &str) -> Option<ObjType> {
    match string {
        "void" => Some(ObjType::Void),

        "bool" => Some(ObjType::BOOL),
        "char" => Some(ObjType::Char),

        "i8"   => Some(ObjType::Integer(IntObjType::I8)),
        "i16"  => Some(ObjType::Integer(IntObjType::I16)),
        "i32"  => Some(ObjType::Integer(IntObjType::I32)),
        "i64"  => Some(ObjType::Integer(IntObjType::I64)),
        "i128" => Some(ObjType::Integer(IntObjType::I128)),
        "isize" => Some(ObjType::Integer(IntObjType::ISize)),

        "u8"   => Some(ObjType::Integer(IntObjType::U8)),
        "u16"  => Some(ObjType::Integer(IntObjType::U16)),
        "u32"  => Some(ObjType::Integer(IntObjType::U32)),
        "u64"  => Some(ObjType::Integer(IntObjType::U64)),
        "u128" => Some(ObjType::Integer(IntObjType::U128)),
        "usize" => Some(ObjType::Integer(IntObjType::USize)),

        "f32" => Some(ObjType::Float(FloatObjType::F32)),
        "f64" => Some(ObjType::Float(FloatObjType::F64)),

        _ => None,
    }
}

fn parse_number_literal(mut string: String) -> Result<TypedExpression, CE> {
    string = string.replace('_', ""); 

    let has_dot = string.find('.').is_some();

    let Some(index) = string.find(|c: char| !c.is_ascii_digit() && c != '.') else {
        return if has_dot {
            // 12.3
            Ok(TypedExpression::new(ObjType::DEFAULT_FLOAT, LinkedExpression::FloatLiteral(string, ObjType::DEFAULT_FLOAT)))
        } else {
            // 123
            Ok(TypedExpression::new(ObjType::DEFAULT_INTEGER, LinkedExpression::IntLiteral(string, ObjType::DEFAULT_INTEGER)))
        }
    };

    let suffix = string.split_off(index);
    let option_type = parse_primitive_type(&suffix);
    let Some(object_type) = option_type else {
        return Err(CE::LiteralParseError { what: string + &suffix, error: format!("unexpected suffix {suffix}") });
    };

    match object_type {
        ObjType::Integer(_) if !has_dot => Ok(TypedExpression::new(object_type.clone(), LinkedExpression::IntLiteral(string, object_type))),
        ObjType::Float(_) => Ok(TypedExpression::new(object_type.clone(), LinkedExpression::FloatLiteral(string, object_type))),
        _ => Err(CE::LiteralParseError { what: string + &suffix, error: format!("unexpected suffix {suffix}") }),
    }
}

fn check_is_returns(statements: &[LinkedStatement]) -> bool {
    for statement in statements {
        match statement {
            LinkedStatement::Return(_) => return true,
            LinkedStatement::If { condition: _, body } | LinkedStatement::While { condition: _, body } => {
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
    fn check_can_cast(from: &Self, other: &Self) -> bool {
        if other == &Self::Char {
            return from == &Self::Integer(IntObjType::U8)
        }
        match from {
            Self::Char => matches!(other, Self::Integer(_)),
            Self::Float(_) => matches!(other, Self::Float(_)),
            Self::Integer(from) => {
                matches!(other, Self::Reference(_)) ||
                matches!(other, Self::Integer(to) if !to.is_bool() || from.is_bool())
            },
            Self::Reference(_) => matches!(other, Self::Reference(_) | Self::Integer(_)),
            Self::Function { .. } | Self::Void => unreachable!(),
        }
    }
    fn from_unary_operation(object_type: &Self, one_sided_operation: OneSidedOperation) -> Option<Self> {
        match one_sided_operation {
            OneSidedOperation::BoolNot => {
                if object_type == &Self::BOOL {
                    Some(Self::BOOL)
                } else {
                    None
                }
            }
            OneSidedOperation::UnaryMinus => {
                if matches!(object_type, Self::Integer(int) if int.is_signed()) || matches!(object_type, Self::Float(_)) {
                    Some(object_type.clone())
                } else {
                    None
                }
            }
            OneSidedOperation::GetReference => {
                Some(Self::Reference(Box::new(object_type.clone())))
            }
            OneSidedOperation::Dereference => {
                if let Self::Reference(reference_object_type) = object_type {
                    Some(*reference_object_type.clone())
                } else {
                    None
                }
            }
        }
    }
    fn from_operation(object_type1: &Self, object_type2: &Self, two_sided_operation: TwoSidedOperation) -> Option<Self> {
        match two_sided_operation {
            TwoSidedOperation::Number(op) => {
                if object_type1 != object_type2 {
                    None
                } else if matches!(object_type1, Self::Integer(_)) || matches!(object_type1, Self::Float(_) if op.can_use_on_float()) {
                    Some(object_type1.clone())
                } else {
                    None
                }
            }
            TwoSidedOperation::Bool(_) => {
                if object_type1 == &Self::BOOL && object_type2 == &Self::BOOL {
                    Some(Self::BOOL)
                } else {
                    None
                }
            }
            TwoSidedOperation::Compare(comp_op) => {
                let is_allowed = if object_type1 != object_type2 {
                    false
                } else if comp_op.is_equal_op() {
                    true
                } else {
                    // reference can't be compared
                    !matches!(object_type1, Self::Reference(_))
                };

                if is_allowed {
                    Some(Self::BOOL)
                } else {
                    None
                }
            }
        }
    }
}

fn check_condition_type(condition: &TypedExpression) -> Result<(), CE> {
    match condition.object_type {
        ObjType::BOOL => Ok(()),
        _ => Err(CE::IncorrectType { got: condition.object_type.clone(), expected: ObjType::BOOL }),
    }
}
