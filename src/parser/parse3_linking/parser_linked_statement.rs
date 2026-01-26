use super::context_window::ObjectContextWindow;
use super::linked_statement::*;
use super::object::{FloatObjType, IntObjType, ObjType, Object};
use crate::error::CResult;
use crate::parser::operations::{OneSidedOperation, TwoSidedOperation};
use crate::parser::parse2_syntactic::statement::*;
use crate::parser::parse3_linking::error::LinkingError;
use crate::parser::parse3_linking::TypeContext;

pub fn link_objects(context: &mut TypeContext) -> CResult<()> {
    let function_declarations = std::mem::take(&mut context.function_statement);
    let variable_declarations = std::mem::take(&mut context.variable_statement);
    let extern_declarations = std::mem::take(&mut context.extern_statements);

    let mut linking_context = FunctionLinkingContext::new(context);

    for (object, statement) in extern_declarations {
        linking_context.link_global_declaration(object, statement)?;
    }
    for (&object, statement) in &variable_declarations {
        linking_context.prelink_global_variable(object, statement)?;
    }
    for (&object, statement) in &function_declarations {
        linking_context.prelink_global_function(object, statement)?;
    }

    for (object, statement) in variable_declarations {
        linking_context.parse_global_variable(object, statement)?;
    }
    for (object, statement) in function_declarations {
        linking_context.parse_global_function(object, statement)?;
    }
    Ok(())
}

struct FunctionLinkingContext<'factory> {
    object_context_window: ObjectContextWindow,
    context: &'factory mut TypeContext,
    current_function_returns: Option<ObjType>,
}

impl<'factory> FunctionLinkingContext<'factory> {
    fn new(context: &'factory mut TypeContext) -> Self {
        Self {
            object_context_window: ObjectContextWindow::new(context.available_names.clone()),
            context,
            current_function_returns: None,
        }
    }
}

impl FunctionLinkingContext<'_> {
    fn link_global_declaration(&mut self, object: Object, statement: Statement) -> CResult<()> {
        let Statement::ExternStatement { statement } = statement else { unreachable!() };
        match statement {
            ExternStatement::Variable { name, typee } => {
                let obj_type = self.parse_type(&typee)?;

                let extern_statement = ExternLinkedStatement::Variable { name: name.clone(), typee: obj_type.clone() };
                let extern_statement = GlobalLinkedStatement::ExternStatement { statement: extern_statement };

                *self.context.factory.get_type_mut(object) = obj_type;
                self.object_context_window.add(name, object);
                self.context.result.extern_statements.insert(object, extern_statement);
                Ok(())
            }
            ExternStatement::Function { name, args, is_vararg, returns } => {
                let arguments_type = args.iter().map(|typee| {
                    let object_type = self.parse_type(typee)?;
                    if object_type.is_void() {
                        LinkingError::UnexpectedVoidUse.print();
                        return Err(())
                    }
                    Ok(object_type)
                }).collect::<Result<Vec<_>, _>>()?;
                let return_type = {
                    match returns.clone() {
                        Some(typee) => self.parse_type(&typee)?,
                        None => ObjType::Void,
                    }
                };
                let func_type = ObjType::Function {
                    arguments: arguments_type,
                    is_vararg,
                    returns: Box::new(return_type.clone()),
                };
                
                let extern_statement = ExternLinkedStatement::Function { name: name.clone(), typee: func_type.clone() };
                let extern_statement = GlobalLinkedStatement::ExternStatement { statement: extern_statement };

                *self.context.factory.get_type_mut(object) = func_type;
                self.object_context_window.add(name, object);
                self.context.result.extern_statements.insert(object, extern_statement);
                Ok(())
            }
        }
    }
    fn prelink_global_variable(&mut self, object: Object, statement: &Statement) -> CResult<()> {
        let Statement::DeclarationStatement { name, statement } = statement else { unreachable!() };
        let DeclarationStatement::VariableDeclaration { typee, value: _ } = statement else { unreachable!() };

        let Some(typee) = typee else {
            LinkingError::GlobalVariableWithoutType { name: name.to_owned() }.print();
            return Err(())
        };
        let obj_type = self.parse_type(typee)?;

        *self.context.factory.get_type_mut(object) = obj_type;
        self.object_context_window.add(name.to_owned(), object);

        Ok(())
    }
    fn parse_global_variable(&mut self, object: Object, statement: Statement) -> CResult<()> {
        let Statement::DeclarationStatement { name: _, statement } = statement else { unreachable!() };
        let DeclarationStatement::VariableDeclaration { typee: _, value } = statement else { unreachable!() };

        let var_type = self.context.factory.get_type(object).clone();

        let expression = self.parse_expression(value, Some(&var_type))?;
        let linked_statement = GlobalLinkedStatement::new_variable(expression);

        self.context.result.variable_statement.insert(object, linked_statement);
        Ok(())
    }
    fn prelink_global_function(&mut self, object: Object, statement: &Statement) -> CResult<()> {
        let Statement::DeclarationStatement { name, statement } = statement else { unreachable!() };
        let DeclarationStatement::Function { args, returns, body: _ } = statement else { unreachable!() };

        let mut arguments_type = Vec::with_capacity(args.len());
        for (_, typee) in args {
            let object_type = self.parse_type(typee)?;
            if object_type.is_void() {
                LinkingError::UnexpectedVoidUse.print();
                return Err(())
            }
            arguments_type.push(object_type);
        }

        let return_type = {
            match returns.clone() {
                Some(typee) => self.parse_type(&typee)?,
                None => ObjType::Void,
            }
        };

        let func_type = ObjType::Function {
            arguments: arguments_type,
            returns: Box::new(return_type.clone()),
            is_vararg: false
        };
        *self.context.factory.get_type_mut(object) = func_type;
        self.object_context_window.add(name.to_owned(), object);

        Ok(())
    }
    fn parse_global_function(&mut self, object: Object, statement: Statement) -> CResult<()> {
        let Statement::DeclarationStatement { name, statement } = statement else { unreachable!() };
        let DeclarationStatement::Function { args, returns: _, body } = statement else { unreachable!() };

        let mut arguments_obj = Vec::with_capacity(args.len());

        let func_type = self.context.factory.get_type(object).clone();
        let ObjType::Function { arguments, is_vararg, returns: return_type } = func_type else { unreachable!() };
        if is_vararg { unreachable!() }
        let return_type = *return_type;

        for (arg_name, _) in args {
            let object_type = arguments[arguments_obj.len()].clone();
            let object = self.context.factory.create_object(arg_name.clone(), object_type);
            self.object_context_window.add(arg_name, object);
            arguments_obj.push(object);
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
                LinkingError::FunctionMustReturn { function_name: name }.print();
                return Err(())
            }
        }

        let linked_statement = GlobalLinkedStatement::new_function(arguments_obj, return_type, body);
        self.context.result.function_statement.insert(object, linked_statement);
        Ok(())
    }
    fn link_statements_recursive(&mut self, statements: Vec<Statement>) -> CResult<Vec<LinkedStatement>> {
        let mut result = Vec::with_capacity(statements.len());
        for statement in statements {
            result.push(self.parse_statement(statement)?);
        }
        Ok(result)
    }
    fn parse_statement(&mut self, statement: Statement) -> CResult<LinkedStatement> {
        match statement {
            Statement::SetVariable { what, value, op } => {
                let what = self.parse_expression(what, None)?;
                if let ObjType::Reference { obj_type: what_ref, is_weak: _ } = &what.object_type {
                    // &T = T
                    let value = self.parse_expression(value, Some(what_ref))?;
                    let new_what = TypedExpression {
                        object_type: what_ref.as_ref().clone(),
                        expr: LinkedExpression::new_unary_operation(what, OneSidedOperation::Dereference)
                    };
                    Ok(LinkedStatement::new_set(new_what, value, op))
                } else {
                    // T = T
                    let value = self.parse_expression(value, Some(&what.object_type))?;
                    Ok(LinkedStatement::new_set(what, value, op))
                }
            }
            Statement::Expression(expression) => {
                let expression = self.parse_expression(expression, None)?;
                Ok(LinkedStatement::Expression(expression))
            }
            Statement::If { condition, body } => {
                let condition = self.parse_expression(condition, Some(&ObjType::BOOL))?;
                self.object_context_window.step_in();
                let body = self.link_statements_recursive(body)?;
                self.object_context_window.step_out();
                Ok(LinkedStatement::new_if(condition, body))
            }
            Statement::While { condition, body } => {
                let condition = self.parse_expression(condition, Some(&ObjType::BOOL))?;
                self.object_context_window.step_in();
                let body = self.link_statements_recursive(body)?;
                self.object_context_window.step_out();
                Ok(LinkedStatement::new_while(condition, body))
            }
            Statement::Return(option_expression) => {
                let expression = match option_expression {
                    Some(expression) => Some(self.parse_expression(expression, self.current_function_returns.clone().as_ref())?),
                    None => None,
                };
                let expression_type = match &expression {
                    Some(expr) => &expr.object_type,
                    None => &ObjType::Void,
                };
                if Some(expression_type) != self.current_function_returns.as_ref() {
                    let expected_type = self.current_function_returns.clone().unwrap_or(ObjType::Void);
                    LinkingError::IncorrectType { got: expression_type.clone(), expected: expected_type }.print();
                    return Err(())
                }
                Ok(LinkedStatement::Return(expression))
            }
            Statement::DeclarationStatement { name, statement} => match &statement {
                DeclarationStatement::VariableDeclaration { .. } => {
                    self.parse_variable_declaration(name, statement)
                },
                DeclarationStatement::Function { .. } | DeclarationStatement::Struct { .. } => unreachable!(),
            },
            Statement::ComptimeStatement(statement) => match statement {
                ComptimeStatement::Import { .. } => unimplemented!("import"),
            },
            Statement::ExternStatement { .. } => unimplemented!("local extern"),
        }
    }
    fn parse_variable_declaration(&mut self, name: String, declaration: DeclarationStatement) -> CResult<LinkedStatement> {
        let DeclarationStatement::VariableDeclaration { typee, value } = declaration else { unreachable!() };
        let objet_type_option = match typee {
            Some(typee) => Some(self.parse_type(&typee)?),
            None => None,
        };
        let typed_expr = self.parse_expression(value, objet_type_option.as_ref())?;
        if typed_expr.object_type.is_void() {
            LinkingError::UnexpectedVoidUse.print();
            return Err(())
        }

        let object = self.context.factory.create_object(name.clone(), typed_expr.object_type.clone());
        self.object_context_window.add(name, object);
        Ok(LinkedStatement::new_variable(object, typed_expr))
    }
    fn try_autocast(mut typed_expression: TypedExpression, expected_type: Option<&ObjType>) -> CResult<TypedExpression> {
        let Some(expected_type) = expected_type else {
            return Ok(typed_expression)
        };
        if &typed_expression.object_type == expected_type {
            return Ok(typed_expression)
        }
        // &T <-> *T
        if typed_expression.object_type.is_different_pointers(expected_type) {
            typed_expression.object_type = expected_type.clone();
            return Ok(typed_expression)
        }
        // &T/&weakT -> T
        if let ObjType::Reference { obj_type, is_weak: _ } = &typed_expression.object_type && expected_type == obj_type.as_ref() {
            return Ok(TypedExpression {
                object_type: expected_type.clone(),
                expr: LinkedExpression::new_unary_operation(typed_expression, OneSidedOperation::Dereference)
            })
        }
        LinkingError::IncorrectType { expected: expected_type.clone(), got: typed_expression.object_type }.print();
        Err(())
    }
    fn try_deref_reference(expression: TypedExpression) -> TypedExpression {
        if !matches!(expression.object_type, ObjType::Reference { .. }) {
            return expression;
        }
        let ObjType::Reference { obj_type, is_weak: _ } = &expression.object_type else { unreachable!() };
        let object_type = obj_type.as_ref().clone();

        TypedExpression {
            object_type,
            expr: LinkedExpression::new_unary_operation(expression, OneSidedOperation::Dereference)
        }
    }
    fn parse_expression(&mut self, expression: Expression, expected_type: Option<&ObjType>) -> CResult<TypedExpression> {
        match expression {
            Expression::Operation(expression1, expression2, op) => {
                let ex1 = self.parse_expression(*expression1, None)?;
                let ex1 = Self::try_deref_reference(ex1);
                let ex2 = self.parse_expression(*expression2, Some(&ex1.object_type))?;
                // ex2 will deref if can in [Self::try_autocast]

                let Some(result_type) = ObjType::from_operation(&ex1.object_type, &ex2.object_type, op) else {
                    LinkingError::IncorrectTwoOper { object_type1: ex1.object_type, object_type2: ex2.object_type, op }.print();
                    return Err(())
                };
                let result = TypedExpression::new(
                    result_type,
                    LinkedExpression::new_operation(ex1, ex2, op)
                );
                Self::try_autocast(result, expected_type)
            }
            Expression::UnaryOperation(expression, op) => {
                let ex = match op {
                    OneSidedOperation::GetReference => {
                        let mut exp = self.parse_expression(*expression, None)?;
                        if let ObjType::Reference { obj_type, is_weak: true } = exp.object_type {
                            // & of &weak T is &T
                            exp.object_type = ObjType::new_reference(*obj_type);
                            return Self::try_autocast(exp, expected_type)
                        } else {
                            exp
                        }
                    }
                    OneSidedOperation::Dereference => {
                        self.parse_expression(*expression, None)?
                    },
                    OneSidedOperation::BoolNot => {
                        let ex = self.parse_expression(*expression, None)?;
                        Self::try_deref_reference(ex)
                    }
                    OneSidedOperation::UnaryMinus => {
                        let ex = self.parse_expression(*expression, expected_type)?;
                        Self::try_deref_reference(ex)
                    },
                };
                let Some(result_type) = ObjType::from_unary_operation(&ex.object_type, op) else {
                    LinkingError::IncorrectOneOper { object_type: ex.object_type, op }.print();
                    return Err(())
                };
                let result = TypedExpression::new(
                    result_type,
                    LinkedExpression::new_unary_operation(ex, op)
                );
                Self::try_autocast(result, expected_type)
            }
            Expression::As(expression, typee) => {
                let ex = self.parse_expression(*expression, None)?;
                let ex = Self::try_deref_reference(ex); // FIXME: maybe not do it for * and &?
                let object_type = self.parse_type(&typee)?;

                let can_cast = ObjType::check_can_cast(&ex.object_type, &object_type);
                if !can_cast {
                    LinkingError::IncorrectAs { what: ex.expr.to_string(), from: ex.object_type, to: object_type }.print();
                    return Err(())
                }

                let result = TypedExpression::new(
                    object_type.clone(),
                    LinkedExpression::new_as(ex, object_type),
                );
                Self::try_autocast(result, expected_type)
            }
            Expression::RoundBracket(ex1) => {
                let ex1 = self.parse_expression(*ex1, expected_type)?;
                Ok(TypedExpression::new(
                    ex1.object_type.clone(),
                    LinkedExpression::new_round_bracket(ex1)
                ))
            }
            Expression::Variable(name) => {
                let object = self.object_context_window.get_or_error(&name)?;
                let obj_type = self.context.factory.get_type(object);

                if matches!(obj_type, ObjType::Function { .. }) {
                    LinkingError::FunctionAsValue { name }.print();
                    return Err(())
                }

                let result = TypedExpression::new(
                    obj_type.clone(),
                    LinkedExpression::Variable(object)
                );
                Self::try_autocast(result, expected_type)
            },
            Expression::FunctionCall { object: name, args: args_values } => {
                let object = self.object_context_window.get_or_error(&name)?;
                let ObjType::Function { arguments, is_vararg, returns } = self.context.factory.get_type(object).clone() else {
                    LinkingError::CallNotFunction { name }.print();
                    return Err(())
                };
                if args_values.len() < arguments.len() || (!is_vararg && args_values.len() > arguments.len()) {
                    let function_name = self.context.factory.get_name(object).clone();
                    LinkingError::IncorrectArgumentCount { function_name, is_vararg, argument_need: arguments.len(), argument_got: args_values.len() }.print();
                    return Err(())
                }
                let args = args_values.into_iter().enumerate()
                    .map(|(index, x)| {
                        if index < arguments.len() {
                            self.parse_expression(x, Some(&arguments[index]))
                        } else {
                            self.parse_expression(x, None)
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                for index in 0..arguments.len() {
                    if args[index].object_type != arguments[index] {
                        LinkingError::IncorrectType { got: args[index].object_type.clone(), expected: arguments[index].clone() }.print();
                        return Err(())
                    }
                }

                let result = TypedExpression::new(
                    returns.as_ref().clone(),
                    LinkedExpression::new_function_call(object, args)
                );
                Self::try_autocast(result, expected_type)
            }
            Expression::StructField { left, field } => {
                let left = self.parse_expression(*left, None)?;

                match &left.object_type {
                    ObjType::Reference{ obj_type: object, is_weak: _ } if matches!(object.as_ref(), ObjType::Struct(..)) => {
                        let ObjType::Struct(object) = object.as_ref() else { unreachable!() };

                        let statement = self.context.result.type_statements.get(object).unwrap();
                        let GlobalLinkedStatement::Struct { fields, field_names } = statement else { unreachable!() };
                        let Some(&index) = field_names.get(&field) else {
                            LinkingError::StructFieldNameNotFound {
                                struct_name: self.context.factory.get_name(*object).clone(),
                                field_name: field,
                            }.print();
                            return Err(());
                        };
                        let obj_type = fields.get(index as usize).unwrap().clone();

                        let result = TypedExpression {
                            object_type: ObjType::new_weak_reference(obj_type),
                            expr: LinkedExpression::new_struct_field(left, index)
                        };
                        Self::try_autocast(result, expected_type)
                    }
                    ObjType::Struct(object) => {
                        let statement = self.context.result.type_statements.get(object).unwrap();
                        let GlobalLinkedStatement::Struct { fields, field_names } = statement else { unreachable!() };
                        let Some(&index) = field_names.get(&field) else {
                            LinkingError::StructFieldNameNotFound {
                                struct_name: self.context.factory.get_name(*object).clone(),
                                field_name: field,
                            }.print();
                            return Err(());
                        };
                        let obj_type = fields.get(index as usize).unwrap().clone();

                        let struct_expr = TypedExpression {
                            object_type: ObjType::new_weak_reference(left.object_type.clone()),
                            expr: LinkedExpression::new_unary_operation(left, OneSidedOperation::GetReference)
                        };
                        let result = TypedExpression {
                            object_type: ObjType::new_weak_reference(obj_type),
                            expr: LinkedExpression::new_struct_field(struct_expr, index)
                        };
                        Self::try_autocast(result, expected_type)
                    }
                    _ => {
                        LinkingError::DotNotOnStruct { got: left.object_type }.print();
                        Err(())
                    }
                }
            }
            Expression::Literal(literal) => self.parse_literal(literal, expected_type),
        }
    }
    fn parse_literal(&mut self, literal: LiteralExpression, expected_type: Option<&ObjType>) -> CResult<TypedExpression> {
        match literal {
            LiteralExpression::Undefined => {
                let Some(obj_type) = expected_type else {
                    LinkingError::CantDetermineType.print();
                    return Err(())
                };
                Ok(TypedExpression::new(
                    obj_type.clone(),
                    LinkedLiteralExpression::Undefined(obj_type.clone()).into(),
                ))
            }
            LiteralExpression::NumberLiteral(string) => {
                let result = parse_number_literal(string, expected_type)?;
                Self::try_autocast(result, expected_type)
            },
            LiteralExpression::BoolLiteral(value) => {
                let result = TypedExpression::new(
                    ObjType::BOOL,
                    LinkedLiteralExpression::BoolLiteral(value).into(),
                );
                Self::try_autocast(result, expected_type)
            }
            LiteralExpression::CharLiteral(value) => {
                let result = TypedExpression::new(
                    ObjType::Char,
                    LinkedLiteralExpression::CharLiteral(value).into(),
                );
                Self::try_autocast(result, expected_type)
            }
            LiteralExpression::StringLiteral(string) => {
                let result = TypedExpression::new(
                    ObjType::new_pointer(ObjType::Char),
                    LinkedLiteralExpression::StringLiteral(string).into(),
                );
                Self::try_autocast(result, expected_type)
            }
        }
    }

    fn parse_type(&self, typee: &Typee) -> CResult<ObjType> {
        match typee {
            Typee::String(string) => {
                let primitive_option = parse_primitive_type(string);
                if let Some(object_type) = primitive_option {
                    return Ok(object_type)
                }

                if let Some(type_object) = self.object_context_window.get(string) {
                    let Some(statement) = self.context.result.type_statements.get(&type_object) else {
                        LinkingError::NameNotFound { name: string.clone(), context: format!("{:?}", self.object_context_window) }.print();
                        return Err(());
                    };
                    match statement {
                        GlobalLinkedStatement::Struct { .. } => return Ok(ObjType::Struct(type_object)),
                        GlobalLinkedStatement::Function { .. }
                        | GlobalLinkedStatement::VariableDeclaration { .. }
                        | GlobalLinkedStatement::ExternStatement { .. } => unreachable!(),
                    }
                };

                LinkingError::NameNotFound { name: string.clone(), context: format!("{:?}", self.object_context_window) }.print();
                Err(())
            }
            Typee::Pointer(obj_type) => {
                let obj_type = self.parse_type(obj_type)?;
                Ok(ObjType::new_pointer(obj_type))
            }
            Typee::Reference(obj_type) => {
                let obj_type = self.parse_type(obj_type)?;
                Ok(ObjType::new_reference(obj_type))
            }
        }
    }
}

pub fn parse_primitive_type(string: &str) -> Option<ObjType> {
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

fn parse_number_literal(mut string: String, expected_type: Option<&ObjType>) -> CResult<TypedExpression> {
    string = string.replace('_', "");

    let has_dot = string.find('.').is_some();

    let Some(index) = string.find(|c: char| !c.is_ascii_digit() && c != '.') else {
        return if has_dot {
            // 12.3
            let mut num_type = ObjType::DEFAULT_FLOAT;
            if let Some(obj_type) = expected_type && matches!(obj_type, ObjType::Float(..)) {
                num_type = obj_type.clone();
            }
            Ok(TypedExpression::new(
                num_type.clone(),
                LinkedLiteralExpression::FloatLiteral(string, num_type).into()
            ))
        } else {
            // 123
            let mut num_type = ObjType::DEFAULT_INTEGER;
            if let Some(obj_type) = expected_type && matches!(obj_type, ObjType::Integer(..)) {
                num_type = obj_type.clone();
            }
            Ok(TypedExpression::new(
                num_type.clone(),
                LinkedLiteralExpression::IntLiteral(string, num_type).into()
            ))
        }
    };

    let suffix = string.split_off(index);
    let option_type = parse_primitive_type(&suffix);
    let Some(object_type) = option_type else {
        LinkingError::LiteralParseError { what: string + &suffix, error: format!("unexpected suffix {suffix}") }.print();
        return Err(())
    };

    match object_type {
        ObjType::Integer(_) if !has_dot => Ok(TypedExpression::new(
            object_type.clone(),
            LinkedLiteralExpression::IntLiteral(string, object_type).into()
        )),
        ObjType::Float(_) => Ok(TypedExpression::new(
            object_type.clone(),
            LinkedLiteralExpression::FloatLiteral(string, object_type).into()
        )),
        _ => {
            LinkingError::LiteralParseError { what: string + &suffix, error: format!("unexpected suffix {suffix}") }.print();
            Err(())
        }
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
            Self::Unknown => false,
            Self::Char => matches!(other, Self::Integer(_)),
            Self::Float(_) => matches!(other, Self::Float(_)),
            Self::Integer(from) => {
                matches!(other, Self::Pointer(_)) ||
                matches!(other, Self::Integer(to) if !to.is_bool() || from.is_bool())
            },
            Self::Pointer(..) =>   matches!(other, Self::Pointer(..) | Self::Reference { .. } | Self::Integer(..)),
            Self::Reference { .. } => matches!(other, Self::Pointer(..) | Self::Reference { .. }),
            Self::Struct(..) | Self::Function { .. } | Self::Void => unreachable!(),
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
                Some(Self::Pointer(Box::new(object_type.clone())))
            }
            OneSidedOperation::Dereference => {
                if let Self::Pointer(reference_object_type) = object_type {
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
                    // pointers can't be compared
                    !matches!(object_type1, Self::Pointer(_) | Self::Reference { .. })
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
