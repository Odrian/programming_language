use crate::context_window::ObjectContextWindow;
use crate::error::LinkingError;
use crate::linked_statement::*;
use crate::object::{parse_primitive_type, IntObjType, ObjType, Object, ObjectFactory};
use crate::{FileLinkingContext, ModuleLiningContext};
use lsp_types::Range;
use pr_ast::statement::*;
use pr_common::error::{Diagnostic, ErrorQueue};
use pr_common::operations::{OneSidedOperation, TwoSidedOperation};
use pr_common::ranged::RString;
use std::cell::RefCell;

pub fn link_objects(errors: &mut ErrorQueue, context: &mut ModuleLiningContext) {
    // TODO: parallelize, the only problem is factory, maybe just clone factory for each file?
    for file in &mut context.files {
        let factory = &mut context.factory;
        link_file_objects(errors, file, factory);
    }
}

pub fn link_file_objects(errors: &mut ErrorQueue, file: &mut FileLinkingContext, factory: &mut ObjectFactory) {
    let function_declarations = std::mem::take(&mut file.function_statement);
    let variable_declarations = std::mem::take(&mut file.variable_statement);
    let extern_declarations = std::mem::take(&mut file.extern_statements);

    let mut linking_context = FunctionLinkingContext::new(errors, file, factory);

    let r1 = extern_declarations.into_iter().map(|(object, statement)|
        linking_context.link_global_declaration(object, statement)
    ).collect::<Result<(), ()>>();
    let r2 = variable_declarations.iter().map(|(&object, statement)|
        linking_context.prelink_global_variable(object, statement)
    ).collect::<Result<(), ()>>();
    let r3 = function_declarations.iter().map(|(&object, statement)|
        linking_context.prelink_global_function(object, statement)
    ).collect::<Result<(), ()>>();

    if r1.is_err() || r2.is_err() || r3.is_err() {
        return;
    }

    let _ = variable_declarations.into_iter().map(|(object, statement)|
        linking_context.parse_global_variable(object, statement)
    ).collect::<Result<(), ()>>();
    let _ = function_declarations.into_iter().map(|(object, statement)|
        linking_context.parse_global_function(object, statement)
    ).collect::<Result<(), ()>>();
}

struct FunctionLinkingContext<'factory> {
    errors: RefCell<&'factory mut ErrorQueue>,
    object_context_window: ObjectContextWindow,
    context: &'factory mut FileLinkingContext,
    factory: &'factory mut ObjectFactory,
    current_function_returns: Option<ObjType>,
}

impl<'factory> FunctionLinkingContext<'factory> {
    fn new(errors: &'factory mut ErrorQueue, context: &'factory mut FileLinkingContext, factory: &'factory mut ObjectFactory) -> Self {
        Self {
            errors: RefCell::new(errors),
            object_context_window: ObjectContextWindow::new(context.available_names.clone()),
            context,
            factory,
            current_function_returns: None,
        }
    }
}

impl FunctionLinkingContext<'_> {
    fn add_diag(&self, diag: Diagnostic) {
        self.errors.borrow_mut().add_diag(diag)
    }
    fn link_global_declaration(&mut self, object: Object, statement: RStatement) -> Result<(), ()> {
        let Statement::ExternStatement { statement } = statement.value else { unreachable!() };
        match statement {
            ExternStatement::Variable { name, typee } => {
                let obj_type = self.parse_type(&typee)?;

                let extern_statement = ExternLinkedStatement::Variable { typee: obj_type.clone() };
                let extern_statement = GlobalLinkedStatement::ExternStatement { statement: extern_statement };

                *self.factory.get_type_mut(object) = obj_type;
                self.object_context_window.add(name, object);
                self.context.result.extern_statements.insert(object, extern_statement);
                Ok(())
            }
            ExternStatement::Function { name, args, is_vararg, returns } => {
                let arguments_type = args.iter().map(|typee| {
                    let object_type = self.parse_type(typee)?;
                    if object_type.is_void() {
                        self.add_diag(
                            LinkingError::unexpected_void_use(typee.range));
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

                let extern_statement = ExternLinkedStatement::Function { typee: func_type.clone() };
                let extern_statement = GlobalLinkedStatement::ExternStatement { statement: extern_statement };

                *self.factory.get_type_mut(object) = func_type;
                self.object_context_window.add(name, object);
                self.context.result.extern_statements.insert(object, extern_statement);
                Ok(())
            }
        }
    }
    fn prelink_global_variable(&mut self, object: Object, statement: &RStatement) -> Result<(), ()> {
        let Statement::DeclarationStatement { name, statement } = &statement.value else { unreachable!() };
        let DeclarationStatement::VariableDeclaration { typee, value: _ } = statement else { unreachable!() };

        let Some(typee) = typee else {
            self.add_diag(
                LinkingError::global_variable_without_type(name.clone()));
            return Err(());
        };
        let obj_type = self.parse_type(typee)?;

        *self.factory.get_type_mut(object) = obj_type;
        self.object_context_window.add(name.clone(), object);

        Ok(())
    }
    fn parse_global_variable(&mut self, object: Object, statement: RStatement) -> Result<(), ()> {
        let Statement::DeclarationStatement { name: _, statement } = statement.value else { unreachable!() };
        let DeclarationStatement::VariableDeclaration { typee: _, value: expr } = statement else { unreachable!() };

        let var_type = self.factory.get_type(object).clone();

        let expression = self.parse_expression(expr, Some(&var_type))?;
        let linked_statement = GlobalLinkedStatement::new_variable(expression);

        self.context.result.variable_statement.insert(object, linked_statement);
        Ok(())
    }
    fn prelink_global_function(&mut self, object: Object, statement: &RStatement) -> Result<(), ()> {
        let Statement::DeclarationStatement { name, statement } = &statement.value else { unreachable!() };
        let DeclarationStatement::Function { args, returns, body: _ } = statement else { unreachable!() };

        let mut arguments_type = Vec::with_capacity(args.len());
        for (_, typee) in args {
            let object_type = self.parse_type(typee)?;
            if object_type.is_void() {
                self.add_diag(
                    LinkingError::unexpected_void_use(typee.range));
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
        *self.factory.get_type_mut(object) = func_type;
        self.object_context_window.add(name.clone(), object);

        Ok(())
    }
    fn parse_global_function(&mut self, object: Object, statement: RStatement) -> Result<(), ()> {
        let Statement::DeclarationStatement { name, statement } = statement.value else { unreachable!() };
        let DeclarationStatement::Function { args, returns: _, body } = statement else { unreachable!() };

        let mut arguments_obj = Vec::with_capacity(args.len());

        let func_type = self.factory.get_type(object).clone();
        let ObjType::Function { arguments, is_vararg, returns: return_type } = func_type else { unreachable!() };
        if is_vararg { unreachable!() }
        let return_type = *return_type;

        for (arg_name, _) in args {
            let object_type = arguments[arguments_obj.len()].clone();
            let object = self.factory.create_object(arg_name.clone(), object_type);
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
                body.push(LinkedStatement::Return(None).add_no_range());
            } else {
                self.add_diag(
                    LinkingError::function_must_return(name));
                return Err(())
            }
        }

        let linked_statement = GlobalLinkedStatement::new_function(arguments_obj, return_type, body);
        self.context.result.function_statement.insert(object, linked_statement);
        Ok(())
    }
    fn link_statements_recursive(&mut self, statements: Vec<RStatement>) -> Result<Vec<RLinkedStatement>, ()> {
        let mut result = Vec::with_capacity(statements.len());
        for statement in statements {
            self.parse_statement(statement, &mut result)?;
        }
        Ok(result)
    }
    fn parse_statement(&mut self, statement: RStatement, result: &mut Vec<RLinkedStatement>) -> Result<(), ()> {
        match statement.value {
            Statement::Brackets(body) => {
                self.object_context_window.step_in();
                for statement in body {
                    self.parse_statement(statement, result)?;
                }
                self.object_context_window.step_out();
                Ok(())
            }
            Statement::SetVariable { what, value, op } => {
                let what = self.parse_expression(what, None)?;
                self.check_lvalue(&what);
                if let ObjType::Reference { obj_type: what_ref, is_weak: _ } = &what.value.object_type {
                    // &T = T
                    let value = self.parse_expression(value, Some(what_ref))?;
                    let what_range = what.range;
                    let new_what = TypedExpression {
                        object_type: what_ref.as_ref().clone(),
                        expr: LinkedExpression::new_unary_operation(what, OneSidedOperation::Dereference.add_no_range())
                    }.add_range(what_range);
                    result.push(LinkedStatement::new_set(new_what, value, op).add_range(statement.range));
                    Ok(())
                } else {
                    // T = T
                    let value = self.parse_expression(value, Some(&what.value.object_type))?;
                    result.push(LinkedStatement::new_set(what, value, op).add_range(statement.range));
                    Ok(())
                }
            }
            Statement::Expression(expression) => {
                let expression = self.parse_expression(expression.add_range(statement.range), None)?;
                result.push(LinkedStatement::Expression(expression).add_range(statement.range));
                Ok(())
            }
            Statement::If { condition, body } => {
                let condition = self.parse_expression(condition, Some(&ObjType::BOOL))?;
                self.object_context_window.step_in();
                let body = self.link_statements_recursive(body)?;
                self.object_context_window.step_out();
                result.push(LinkedStatement::new_if(condition, body).add_range(statement.range));
                Ok(())
            }
            Statement::While { condition, body } => {
                let condition = self.parse_expression(condition, Some(&ObjType::BOOL))?;
                self.object_context_window.step_in();
                let body = self.link_statements_recursive(body)?;
                self.object_context_window.step_out();
                result.push(LinkedStatement::new_while(condition, body).add_range(statement.range));
                Ok(())
            }
            Statement::Return(option_expression) => {
                let expression = match option_expression {
                    Some(expression) => Some(
                        self.parse_expression(expression, self.current_function_returns.clone().as_ref())?
                    ),
                    None => None,
                };
                let expression_type = match &expression {
                    Some(expr) => &expr.value.object_type,
                    None => &ObjType::Void,
                };
                if Some(expression_type) != self.current_function_returns.as_ref() {
                    let expected_type = self.current_function_returns.clone().unwrap_or(ObjType::Void);
                    self.add_diag(LinkingError::incorrect_type(
                        expression_type.to_str(self), expected_type.to_str(self),
                        statement.range));
                    return Err(())
                }
                result.push(LinkedStatement::Return(expression).add_range(statement.range));
                Ok(())
            }
            Statement::DeclarationStatement { name, statement } => match &statement {
                DeclarationStatement::VariableDeclaration { .. } => {
                    self.parse_variable_declaration(name, statement, result)?;
                    Ok(())
                },
                DeclarationStatement::Function { .. } | DeclarationStatement::Struct { .. } => unreachable!(),
            },
            Statement::ComptimeStatement(statement) => match statement {
                ComptimeStatement::Import { .. } => unimplemented!("import"),
            },
            Statement::ExternStatement { .. } => unimplemented!("local extern"),
        }
    }
    fn parse_variable_declaration(&mut self, name: RString, declaration: DeclarationStatement, result: &mut Vec<RLinkedStatement>) -> Result<(), ()> {
        let DeclarationStatement::VariableDeclaration { typee, value: expr } = declaration else { unreachable!() };
        let objet_type_option = match typee {
            Some(typee) => Some(self.parse_type(&typee)?),
            None => None,
        };
        let expr_range = expr.range;
        let typed_expr = self.parse_expression(expr, objet_type_option.as_ref())?;
        if typed_expr.value.object_type.is_void() {
            self.add_diag(
                LinkingError::unexpected_void_use(expr_range));
            return Err(())
        }

        let object = self.factory.create_object(name.clone(), typed_expr.value.object_type.clone());
        let name_range = name.range;
        self.object_context_window.add(name, object);
        result.push(LinkedStatement::new_variable(object, typed_expr).add_range(name_range));
        Ok(())
    }
    fn try_autocast(&self, mut typed_expression: RTypedExpression, expected_type: Option<&ObjType>) -> Result<RTypedExpression, ()> {
        let Some(expected_type) = expected_type else {
            return Ok(typed_expression)
        };
        if &typed_expression.value.object_type == expected_type {
            return Ok(typed_expression)
        }
        // &T <-> *T
        if typed_expression.value.object_type.is_different_pointers(expected_type) {
            typed_expression.value.object_type = expected_type.clone();
            return Ok(typed_expression)
        }
        // &T/&weakT -> T
        if let ObjType::Reference { obj_type, is_weak: _ } = &typed_expression.value.object_type && expected_type == obj_type.as_ref() {
            let expression_range = typed_expression.range;
            return Ok(TypedExpression {
                object_type: expected_type.clone(),
                expr: LinkedExpression::new_unary_operation(typed_expression, OneSidedOperation::Dereference.add_no_range())
            }.add_range(expression_range))
        }
        self.add_diag(LinkingError::incorrect_type(
            expected_type.to_str(self), typed_expression.value.object_type.to_str(self),
            typed_expression.range));
        Err(())
    }
    fn try_deref_reference(expression: RTypedExpression) -> RTypedExpression {
        if !matches!(expression.value.object_type, ObjType::Reference { .. }) {
            return expression;
        }
        let ObjType::Reference { obj_type, is_weak: _ } = &expression.value.object_type else { unreachable!() };
        let object_type = obj_type.as_ref().clone();

        let expression_range = expression.range;
        TypedExpression {
            object_type,
            expr: LinkedExpression::new_unary_operation(expression, OneSidedOperation::Dereference.add_no_range())
        }.add_range(expression_range)
    }
    fn parse_expression(&self, expression0: RExpression, expected_type: Option<&ObjType>) -> Result<RTypedExpression, ()> {
        match expression0.value {
            Expression::Operation(expression1, expression2, op) => {
                let ex1 = self.parse_expression(*expression1, None)?;
                let ex1 = Self::try_deref_reference(ex1);
                let ex2 = self.parse_expression(*expression2, Some(&ex1.value.object_type))?;
                // ex2 will deref if can in [self.try_autocast]

                let Some(result_type) = ObjType::from_operation(&ex1.value.object_type, &ex2.value.object_type, op.value) else {
                    self.add_diag(LinkingError::incorrect_two_oper(
                        ex1.value.object_type.to_str(self), ex2.value.object_type.to_str(self), op));
                    return Err(())
                };
                let result = TypedExpression::new(
                    result_type,
                    LinkedExpression::new_operation(ex1, ex2, op)
                ).add_range(expression0.range);
                self.try_autocast(result, expected_type)
            }
            Expression::UnaryOperation(expression, op) => {
                let ex = match op.value {
                    OneSidedOperation::GetReference => {
                        let mut exp = self.parse_expression(*expression, None)?;
                        if let ObjType::Reference { obj_type, is_weak: true } = exp.value.object_type {
                            // & of &weak T is &T
                            exp.value.object_type = ObjType::new_reference(*obj_type);
                            return self.try_autocast(exp, expected_type)
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
                let Some(result_type) = ObjType::from_unary_operation(&ex.value.object_type, op.value) else {
                    self.add_diag(LinkingError::incorrect_one_oper(ex.value.object_type.to_str(self), op));
                    return Err(())
                };
                let result = TypedExpression::new(
                    result_type,
                    LinkedExpression::new_unary_operation(ex, op)
                ).add_range(expression0.range);
                self.try_autocast(result, expected_type)
            }
            Expression::As(expression, typee) => {
                let expr_range = expression.range;
                let ex = self.parse_expression(*expression, None)?;
                let ex = Self::try_deref_reference(ex); // FIXME: maybe not do it for * and &?
                let object_type = self.parse_type(&typee)?;

                let can_cast = ObjType::check_can_cast(&ex.value.object_type, &object_type);
                if !can_cast {
                    self.add_diag(LinkingError::incorrect_as(
                        ex.value.object_type.to_str(self), object_type.to_str(self),
                        expr_range));
                    return Err(())
                }

                let result = TypedExpression::new(
                    object_type.clone(),
                    LinkedExpression::new_as(ex, object_type),
                ).add_range(expression0.range);
                self.try_autocast(result, expected_type)
            }
            Expression::RoundBracket(ex1) => {
                let ex1 = self.parse_expression(*ex1, expected_type)?;
                Ok(TypedExpression::new(
                    ex1.value.object_type.clone(),
                    LinkedExpression::new_round_bracket(ex1)
                ).add_range(expression0.range))
            }
            Expression::Variable(name) => {
                let object = self.object_context_window.get_or_error(&name)
                    .map_err(|d| self.add_diag(d))?;
                let obj_type = self.factory.get_type(object);

                if matches!(obj_type, ObjType::Function { .. }) {
                    self.add_diag(LinkingError::function_as_value(name));
                    return Err(())
                }

                let result = TypedExpression::new(
                    obj_type.clone(),
                    LinkedExpression::Variable(object)
                ).add_range(expression0.range);
                self.try_autocast(result, expected_type)
            },
            Expression::FunctionCall { object: name, args: args_values } => {
                let object = self.object_context_window.get_or_error(&name)
                    .map_err(|d| self.add_diag(d))?;
                let ObjType::Function { arguments, is_vararg, returns } = self.factory.get_type(object).clone() else {
                    self.add_diag(LinkingError::call_not_function(name));
                    return Err(())
                };
                if args_values.len() < arguments.len() || (!is_vararg && args_values.len() > arguments.len()) {
                    let function_name = self.factory.get_name(object).clone();
                    self.add_diag(LinkingError::incorrect_argument_count(function_name, is_vararg, arguments.len(), args_values.len()));
                    return Err(())
                }
                let args_ranges: Vec<Range> = args_values.iter().map(|x| x.range).collect();
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
                    if args[index].value.object_type != arguments[index] {
                        self.add_diag(LinkingError::incorrect_type(
                            args[index].value.object_type.to_str(self), arguments[index].to_str(self),
                            args_ranges[index]));
                        return Err(())
                    }
                }

                let result = TypedExpression::new(
                    returns.as_ref().clone(),
                    LinkedExpression::new_function_call(object, args)
                ).add_range(expression0.range);
                self.try_autocast(result, expected_type)
            }
            Expression::StructField { left, field } => {
                let left = self.parse_expression(*left, None)?;

                match &left.value.object_type {
                    ObjType::Reference { obj_type: object, is_weak: _ } if matches!(object.as_ref(), ObjType::Struct(..)) => {
                        let ObjType::Struct(object) = object.as_ref() else { unreachable!() };

                        let statement = self.context.result.type_statements.get(object).unwrap();
                        let GlobalLinkedStatement::Struct { fields, field_names } = statement else { unreachable!() };
                        let Some(&index) = field_names.get(&field.value) else {
                            self.add_diag(LinkingError::struct_field_name_not_found(
                                self.factory.get_name(*object).value.clone(), field));
                            return Err(())
                        };
                        let obj_type = fields.get(index as usize).unwrap().clone();

                        let result = TypedExpression {
                            object_type: ObjType::new_weak_reference(obj_type),
                            expr: LinkedExpression::new_struct_field(left, index)
                        }.add_range(expression0.range);
                        self.try_autocast(result, expected_type)
                    }
                    ObjType::Struct(object) => {
                        let statement = self.context.result.type_statements.get(object).unwrap();
                        let GlobalLinkedStatement::Struct { fields, field_names } = statement else { unreachable!() };
                        let Some(&index) = field_names.get(&field.value) else {
                            self.add_diag(LinkingError::struct_field_name_not_found(
                                self.factory.get_name(*object).value.clone(),
                                field));
                            return Err(())
                        };
                        let obj_type = fields.get(index as usize).unwrap().clone();

                        let struct_expr = TypedExpression {
                            object_type: ObjType::new_weak_reference(left.value.object_type.clone()),
                            expr: LinkedExpression::new_unary_operation(left, OneSidedOperation::GetReference.add_no_range())
                        }.add_range(expression0.range);
                        let result = TypedExpression {
                            object_type: ObjType::new_weak_reference(obj_type),
                            expr: LinkedExpression::new_struct_field(struct_expr, index)
                        }.add_range(expression0.range);
                        self.try_autocast(result, expected_type)
                    }
                    _ => {
                        self.add_diag(LinkingError::dot_not_on_struct(left.value.object_type.to_str(self), field.range));
                        Err(())
                    }
                }
            }
            Expression::Literal(literal) => self.parse_literal(literal, expression0.range, expected_type),
            Expression::StructConstruct { struct_name, fields: fields_values } => {
                let struct_type = self.parse_type(&Typee::String(struct_name.value.clone()).add_range(struct_name.range))?;
                let ObjType::Struct(object) = &struct_type else {
                    self.add_diag(LinkingError::name_not_found(struct_name));
                    return Err(())
                };
                let object = *object;

                let GlobalLinkedStatement::Struct { fields: field_types, field_names } =
                    self.context.result.type_statements.get(&object).unwrap() else { unreachable!() };

                let bad = fields_values.len() != field_types.len();

                let mut fields = vec![None; field_types.len()];
                let mut used_fields = vec![false; field_types.len()];
                for (field_name, value) in fields_values {
                    let Some(index) = field_names.get(&field_name.value) else {
                        self.add_diag(LinkingError::struct_field_name_not_found(struct_name.value, field_name));
                        return Err(())
                    };
                    let index = *index as usize;

                    let expected_type = &field_types[index];
                    let value = self.parse_expression(value, Some(expected_type))?;
                    fields[index] = Some(value);

                    if used_fields[index] {
                        self.add_diag(LinkingError::struct_field_name_collision_in_construction(struct_name, field_name.value));
                        return Err(())
                    }
                    used_fields[index] = true;
                }
                if bad {
                    let index = used_fields.iter().position(|x| !*x).unwrap();
                    let (field_name, _) = field_names.iter().find(|(_, i)| **i == index as u32).unwrap();
                    let field_name = field_name.clone();
                    self.add_diag(LinkingError::struct_field_missing_in_construction(struct_name, field_name));
                    return Err(())
                }

                let fields = fields.into_iter().map(Option::unwrap).collect::<Vec<_>>();

                let result = TypedExpression::new(
                    ObjType::new_weak_reference(struct_type),
                    LinkedExpression::new_struct_construction(
                        object,
                        fields,
                    )
                ).add_range(expression0.range);
                self.try_autocast(result, expected_type)
            }
        }
    }
    fn parse_literal(&self, literal: LiteralExpression, literal_range: Range, expected_type: Option<&ObjType>) -> Result<RTypedExpression, ()> {
        match literal {
            LiteralExpression::Undefined { is_zeroed } => {
                let Some(obj_type) = expected_type else {
                    self.add_diag(LinkingError::cant_determine_type(literal_range));
                    return Err(())
                };
                Ok(TypedExpression::new(
                    obj_type.clone(),
                    LinkedLiteralExpression::Undefined { obj_type: obj_type.clone(), is_zeroed }.into(),
                ).add_range(literal_range))
            }
            LiteralExpression::NumberLiteral(string) => {
                let result = self.parse_number_literal(string, literal_range, expected_type)?;
                self.try_autocast(result, expected_type)
            },
            LiteralExpression::BoolLiteral(value) => {
                let result = TypedExpression::new(
                    ObjType::BOOL,
                    LinkedLiteralExpression::BoolLiteral(value).into(),
                ).add_range(literal_range);
                self.try_autocast(result, expected_type)
            }
            LiteralExpression::CharLiteral(value) => {
                let result = TypedExpression::new(
                    ObjType::Char,
                    LinkedLiteralExpression::CharLiteral(value).into(),
                ).add_range(literal_range);
                self.try_autocast(result, expected_type)
            }
            LiteralExpression::StringLiteral(string) => {
                let result = TypedExpression::new(
                    ObjType::new_pointer(ObjType::Char),
                    LinkedLiteralExpression::StringLiteral(string).into(),
                ).add_range(literal_range);
                self.try_autocast(result, expected_type)
            }
        }
    }

    fn parse_type(&self, typee: &RTypee) -> Result<ObjType, ()> {
        match &typee.value {
            Typee::String(string) => {
                let primitive_option = parse_primitive_type(string);
                if let Some(object_type) = primitive_option {
                    return Ok(object_type)
                }

                if let Some(type_object) = self.object_context_window.get(string) {
                    let Some(statement) = self.context.result.type_statements.get(&type_object) else {
                        self.add_diag(LinkingError::name_not_found(RString::new(string.clone(), typee.range)));
                        return Err(())
                    };
                    match statement {
                        GlobalLinkedStatement::Struct { .. } => return Ok(ObjType::Struct(type_object)),
                        GlobalLinkedStatement::Function { .. }
                        | GlobalLinkedStatement::VariableDeclaration { .. }
                        | GlobalLinkedStatement::ExternStatement { .. } => unreachable!(),
                    }
                };

                self.add_diag(LinkingError::name_not_found(RString::new(string.clone(), typee.range)));
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

    fn parse_number_literal(&self, mut string: String, literal_range: Range, expected_type: Option<&ObjType>) -> Result<RTypedExpression, ()> {
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
                ).add_range(literal_range))
            } else {
                // 123
                let num_type: ObjType;
                if let Some(obj_type) = expected_type && matches!(obj_type, ObjType::Integer(..)) {
                    num_type = obj_type.clone();
                } else {
                    num_type = ObjType::DEFAULT_INTEGER
                }
                Ok(TypedExpression::new(
                    num_type.clone(),
                    LinkedLiteralExpression::IntLiteral(string, num_type).into()
                ).add_range(literal_range))
            }
        };

        let suffix = string.split_off(index);
        let option_type = parse_primitive_type(&suffix);
        let Some(object_type) = option_type else {
            self.add_diag(LinkingError::literal_unexpected_suffix(suffix, literal_range));
            return Err(());
        };

        match object_type {
            ObjType::Integer(_) if !has_dot => Ok(TypedExpression::new(
                object_type.clone(),
                LinkedLiteralExpression::IntLiteral(string, object_type).into()
            ).add_range(literal_range)),
            ObjType::Float(_) => Ok(TypedExpression::new(
                object_type.clone(),
                LinkedLiteralExpression::FloatLiteral(string, object_type).into()
            ).add_range(literal_range)),
            _ => {
                self.add_diag(LinkingError::literal_unexpected_suffix(suffix, literal_range));
                Err(())
            }
        }
    }

    fn check_lvalue(&mut self, lvalue: &RTypedExpression) {
        match &lvalue.value.expr {
            LinkedExpression::Variable(..) => {},
            LinkedExpression::StructField {..} => {}
            LinkedExpression::RoundBracket(inside) => self.check_lvalue(inside),
            LinkedExpression::UnaryOperation(inside, op)
                if op.value == OneSidedOperation::Dereference => self.check_lvalue(inside),
            _ => {
                self.add_diag(LinkingError::incorrect_lvalue(lvalue.range))
            }
        }
    }
}

fn check_is_returns(statements: &[RLinkedStatement]) -> bool {
    for statement in statements {
        match &statement.value {
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
    fn to_str(&self, context: &FunctionLinkingContext) -> String {
        self.to_string::<false>(&context.factory)
    }
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
                match object_type {
                    Self::Pointer(obj_type) => Some(obj_type.as_ref().clone()),
                    Self::Reference { obj_type, is_weak: false } => Some(obj_type.as_ref().clone()),
                    _ => None
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
