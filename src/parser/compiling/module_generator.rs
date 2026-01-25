use super::error::LLVMError;

use crate::parser::operations::*;
use crate::parser::parse3_linking::object::*;
use crate::parser::parse3_linking::linked_statement::*;
use super::context_window::ValueContextWindow;

use std::cmp::Ordering;
use std::collections::HashMap;
use inkwell::values::{BasicValueEnum, BasicMetadataValueEnum, FunctionValue, PointerValue, AnyValue, BasicValue};
use inkwell::{builder::Builder, context::Context, module::Module, AddressSpace, FloatPredicate, IntPredicate};
use inkwell::targets::TargetData;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, StructType};
use crate::parser::parse3_linking::LinkedProgram;

pub fn parse_module<'ctx>(
    context: &'ctx Context, target_data: &'ctx TargetData,
    linked_program: LinkedProgram
) -> Result<Module<'ctx>, LLVMError> {
    let mut code_module_gen = CodeModuleGen::new(context, target_data, linked_program, "main_module");
    code_module_gen.parse_module()?;
    Ok(code_module_gen.module)
}

struct CodeModuleGen<'ctx> {
    context: &'ctx Context,
    target_data: &'ctx TargetData,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    linked_program: LinkedProgram,
    current_function: Option<FunctionValue<'ctx>>,
    context_window: ValueContextWindow<'ctx>,
    struct_context: HashMap<Object, StructType<'ctx>>
}
impl<'ctx> CodeModuleGen<'ctx> {
    fn new(context: &'ctx Context, target_data: &'ctx TargetData, linked_program: LinkedProgram, name: &str) -> Self {
        let module = context.create_module(name);
        let builder = context.create_builder();
        let context_window = ValueContextWindow::new();
        Self {
            context,
            target_data,
            module,
            builder,
            linked_program,
            current_function: None,
            context_window,
            struct_context: Default::default(),
        }
    }
}

impl<'ctx> CodeModuleGen<'ctx> {
    fn get_object_name(&self, object: Object) -> &String {
        self.linked_program.factory.get_name(object)
    }
    fn get_object_type(&self, object: Object) -> BasicTypeEnum<'ctx> {
        self.parse_type(self.linked_program.factory.get_type(object))
    }
    fn parse_type(&self, object_type: &ObjType) -> BasicTypeEnum<'ctx> {
        match object_type {
            ObjType::Unknown => unreachable!(),
            ObjType::Void => unimplemented!(),
            ObjType::Pointer(..) | ObjType::Reference(..) => self.context.ptr_type(AddressSpace::default()).into(),
            ObjType::Char => self.context.i8_type().into(),
            ObjType::Integer(int) => match int {
                IntObjType::Bool => self.context.bool_type().into(),
                IntObjType::I8 | IntObjType::U8 => self.context.i8_type().into(),
                IntObjType::I16 | IntObjType::U16 => self.context.i16_type().into(),
                IntObjType::I32 | IntObjType::U32 => self.context.i32_type().into(),
                IntObjType::I64 | IntObjType::U64 => self.context.i64_type().into(),
                IntObjType::I128 | IntObjType::U128 => self.context.i128_type().into(),
                IntObjType::ISize | IntObjType::USize => {
                    let pointer_size_bytes = self.target_data.get_pointer_byte_size(None);
                    match pointer_size_bytes {
                        4 => self.context.i32_type().into(),
                        8 => self.context.i64_type().into(),
                        _ => panic!("unexpected pointer size: {pointer_size_bytes} bytes")
                    }
                },
            }
            ObjType::Float(float) => match float {
                FloatObjType::F32 => self.context.f32_type().into(),
                FloatObjType::F64 => self.context.f64_type().into(),
            }
            ObjType::Struct(object) => self.struct_context.get(object).copied().unwrap().into(),
            ObjType::Function { .. } => panic!("function type can't be BasicTypeEnum"),
        }
    }
    fn function_from(&self, returns: &ObjType, args: &[BasicMetadataTypeEnum<'ctx>], is_var_args: bool) -> FunctionType<'ctx> {
        if returns == &ObjType::Void {
            self.context.void_type().fn_type(args, is_var_args)
        } else {
            self.parse_type(returns).fn_type(args, is_var_args)
        }
    }
}

fn get_undef(value_type: BasicTypeEnum) -> BasicValueEnum {
    match value_type {
        BasicTypeEnum::ArrayType(v) => v.get_undef().into(),
        BasicTypeEnum::FloatType(v) => v.get_undef().into(),
        BasicTypeEnum::IntType(v) => v.get_undef().into(),
        BasicTypeEnum::PointerType(v) => v.get_undef().into(),
        BasicTypeEnum::StructType(v) => v.get_undef().into(),
        BasicTypeEnum::VectorType(v) => v.get_undef().into(),
        BasicTypeEnum::ScalableVectorType(v) => v.get_undef().into(),
    }
}

/// parsing statements in global space
mod module_parsing {
    use super::*;

    impl<'ctx> CodeModuleGen<'ctx> {
        pub fn parse_module(&mut self) -> Result<(), LLVMError> {
            self.context_window.step_in();

            self.parse_type_statements();

            let extern_statements = std::mem::take(&mut self.linked_program.extern_statements);
            for (object, statement) in extern_statements {
                self.create_extern(object, statement);
            }

            let function_statements = std::mem::take(&mut self.linked_program.function_statement);
            let variable_statements = std::mem::take(&mut self.linked_program.variable_statement);

            // init global context
            for (object, statement) in &function_statements {
                self.create_function(*object, statement);
            }
            for (object, statement) in variable_statements {
                self.create_global_var(object, statement)?;
            }

            // parse declarations
            for (object, statement) in function_statements {
                self.parse_function(object, statement)?;
            }

            self.context_window.step_out();
            Ok(())
        }

        fn create_global_var(&mut self, object: Object, statement: GlobalLinkedStatement) -> Result<(), LLVMError> {
            let GlobalLinkedStatement::VariableDeclaration { value } = statement else { unreachable!() };

            let global_type = self.parse_type(&value.object_type);

            let name = self.get_object_name(object);
            let global_value = self.module.add_global(global_type, None, name);

            let global_const_value = self.get_const_value(object, value)?;
            global_value.set_initializer(&global_const_value);

            self.context_window.add(object, global_value.as_any_value_enum());
            Ok(())
        }
        fn get_const_value(&self, object: Object, value: TypedExpression) -> Result<BasicValueEnum<'ctx>, LLVMError> {
            let TypedExpression { object_type: _, expr } = value;
            let LinkedExpression::Literal(LinkedLiteralExpression::Undefined(obj_type)) = expr else {
                return Err(LLVMError::GlobalWithValue { name: self.get_object_name(object).clone()});
            };
            Ok(get_undef(self.parse_type(&obj_type)))
        }
        fn create_function(&mut self, object: Object, statement: &GlobalLinkedStatement) {
            let GlobalLinkedStatement::Function { args, returns, body: _ } = statement else { unreachable!() };

            let arguments_types: Vec<BasicMetadataTypeEnum> = args.iter().map(|obj| self.get_object_type(*obj).into()).collect();
            let fn_type = self.function_from(returns, &arguments_types, false);

            let name = self.get_object_name(object);
            let function = self.module.add_function(name.as_str(), fn_type, None);
            self.context_window.add(object, function.into());
        }
        fn parse_function(&mut self, object: Object, statement: GlobalLinkedStatement) -> Result<(), LLVMError> {
            let GlobalLinkedStatement::Function { args, returns: _, body } = statement else { unreachable!() };
            let function = self.context_window.get_function_unwrap(object);

            let entry = self.context.append_basic_block(function, "entry");
            self.builder.position_at_end(entry);
            self.context_window.step_in();

            for (index, object) in args.into_iter().enumerate() {
                let arg_value = function.get_nth_param(index as u32).unwrap();
                let arg_type = self.get_object_type(object);
                let pointer = self.builder.build_alloca(arg_type, &format!("arg{index}"))?;
                self.builder.build_store(pointer, arg_value)?;
                self.context_window.add(object, pointer.into());
            }

            self.current_function = Some(function);
            self.parse_function_body(body)?;
            if !function.verify(true) {
                return Err(LLVMError::LLVMVerifyFunctionError { name: self.linked_program.factory.get_name(object).clone() });
            }
            self.current_function = None;

            self.context_window.step_out();
            Ok(())
        }
        fn create_extern(&mut self, object: Object, statement: GlobalLinkedStatement) {
            let GlobalLinkedStatement::ExternStatement { statement } = statement else { unreachable!() };

            match statement {
                ExternLinkedStatement::Variable { name, typee } => {
                    let basic_type = self.parse_type(&typee);

                    let global = self.module.add_global(basic_type, None, &name);
                    global.set_externally_initialized(true);

                    self.context_window.add(object, global.as_any_value_enum());
                }
                ExternLinkedStatement::Function { name, typee } => {
                    let ObjType::Function { arguments, returns } = typee else { unreachable!() };
                    let arguments = arguments.iter().map(|a| self.parse_type(a).into()).collect::<Vec<_>>();
                    let fn_type = self.function_from(returns.as_ref(), &arguments, false);
                    
                    let function = self.module.add_function(&name, fn_type, None);

                    self.context_window.add(object, function.into());
                }
            }
        }
    }
}

mod type_parsing {
    use super::*;

    impl<'ctx> CodeModuleGen<'ctx> {
        pub fn parse_type_statements(&mut self) {
            let mut type_statements = std::mem::take(&mut self.linked_program.type_statements);
            let type_statements_order = std::mem::take(&mut self.linked_program.type_statements_order);

            for object in type_statements_order {
                let statement = type_statements.remove(&object).unwrap();
                match &statement {
                    GlobalLinkedStatement::Struct { .. } => self.parse_struct_statement(object, statement),
                    GlobalLinkedStatement::VariableDeclaration { .. }
                    | GlobalLinkedStatement::Function { .. }
                    | GlobalLinkedStatement::ExternStatement { .. } => unreachable!(),
                }
            }
        }

        fn parse_struct_statement(&mut self, object: Object, statement: GlobalLinkedStatement) {
            let GlobalLinkedStatement::Struct { fields, field_names: _ } = statement else { unreachable!() };

            let fields_types: Vec<_> = fields.iter()
                .map(|obj_type| self.parse_type(obj_type))
                .collect();

            let struct_type = self.context.struct_type(&fields_types, false);
            self.struct_context.insert(object, struct_type);
        }
    }
}

/// parsing functions and global variables
mod declaration_parsing {
    use super::*;

    impl<'ctx> CodeModuleGen<'ctx> {
        pub fn parse_function_body(&mut self, body: Vec<LinkedStatement>) -> Result<(), LLVMError> {
            let is_returned = self.parse_statements(body)?;
            assert!(is_returned);

            Ok(())
        }
        fn parse_statements(&mut self, statements: Vec<LinkedStatement>) -> Result<bool, LLVMError> {
            for statement in statements {
                if self.parse_statement(statement)? {
                    return Ok(true)
                }
            }
            Ok(false)
        }

        fn parse_statement(&mut self, statement: LinkedStatement) -> Result<bool, LLVMError> {
            match statement {
                LinkedStatement::Expression(expression) => {
                    if expression.object_type.is_void() {
                        let LinkedExpression::FunctionCall { object, args } = expression.expr else { unreachable!() };
                        self.call_function(object, args)?;
                    } else {
                        self.parse_expression(expression.expr)?;
                    }
                }
                LinkedStatement::If { condition, body } => {
                    let condition_value = self.parse_expression(condition.expr)?.unwrap().into_int_value();

                    let function = self.current_function.unwrap();
                    let then_bb = self.context.append_basic_block(function, "then");
                    let merge_bb = self.context.append_basic_block(function, "merge");
                    self.builder.build_conditional_branch(condition_value, then_bb, merge_bb)?;

                    self.builder.position_at_end(then_bb);
                    let has_return = self.parse_statements(body)?;
                    if !has_return {
                        self.builder.build_unconditional_branch(merge_bb)?;
                    }
                    self.builder.position_at_end(merge_bb);
                }
                LinkedStatement::While { condition, body } => {
                    let function = self.current_function.unwrap();
                    let cond_bb = self.context.append_basic_block(function, "cond");
                    let body_bb = self.context.append_basic_block(function, "then");
                    let after_bb = self.context.append_basic_block(function, "merge");

                    self.builder.build_unconditional_branch(cond_bb)?;

                    self.builder.position_at_end(cond_bb);
                    let condition_value = self.parse_expression(condition.expr)?.unwrap().into_int_value();
                    self.builder.build_conditional_branch(condition_value, body_bb, after_bb)?;

                    self.builder.position_at_end(body_bb);
                    let has_return = self.parse_statements(body)?;
                    if has_return {
                        return Ok(true)
                    }
                    self.builder.build_unconditional_branch(cond_bb)?;
                    self.builder.position_at_end(after_bb);
                }
                LinkedStatement::SetVariable { what, value, op } => {
                    if let Some(op) = op {
                        let what_ty = self.parse_type(&what.object_type);
                        let obj_ty = what.object_type.clone();
                        let pointer = self.parse_lvalue(what)?;
                        let rvalue = self.parse_expression(value.expr)?.unwrap();
                        let value0 = self.builder.build_load(what_ty, pointer, "load for op-set")?;
                        let rvalue = self.parse_operation(value0, rvalue, obj_ty, op)?;
                        self.builder.build_store(pointer, rvalue)?;
                    } else {
                        let pointer = self.parse_lvalue(what)?;
                        let rvalue = self.parse_expression(value.expr)?.unwrap();
                        self.builder.build_store(pointer, rvalue)?;
                    }
                }
                LinkedStatement::VariableDeclaration { object, value } => {
                    let value = self.parse_expression(value.expr)?.unwrap();
                    let var_type = self.get_object_type(object);
                    let pointer = self.builder.build_alloca(var_type, self.get_object_name(object).as_str())?;
                    self.builder.build_store(pointer, value)?;
                    self.context_window.add(object, pointer.into());
                }
                LinkedStatement::Return(expression) => {
                    if let Some(expression) = expression {
                        let expression = self.parse_expression(expression.expr)?.unwrap();
                        self.builder.build_return(Some(&expression))?;
                    } else {
                        self.builder.build_return(None)?;
                    }
                    return Ok(true)
                }
            }
            Ok(false)
        }
        fn parse_lvalue(&self, expression: TypedExpression) -> Result<PointerValue<'ctx>, LLVMError> {
            if let LinkedExpression::UnaryOperation(expr, OneSidedOperation::Dereference) = expression.expr {
                let value = self.parse_expression(expr.expr)?;
                Ok(value.unwrap().into_pointer_value())
            } else {
                Ok(self.get_pointer(&expression.expr)?.unwrap())
            }
        }

        /// get pointer to local object or None
        fn get_pointer(&self, expression: &LinkedExpression) -> Result<Option<PointerValue<'ctx>>, LLVMError> {
            match expression {
                LinkedExpression::RoundBracket(exp) => self.get_pointer(&exp.expr),
                LinkedExpression::Variable(object) => {
                    let pointer = self.context_window.get_pointer_unwrap(*object);
                    Ok(Some(pointer))
                }
                _ => Ok(None)
            }
        }

        fn parse_expression(&self, expression: LinkedExpression) -> Result<Option<BasicValueEnum<'ctx>>, LLVMError> {
            match expression {
                LinkedExpression::FunctionCall { object, args } => {
                    Ok(Some(self.call_function(object, args)?.unwrap()))
                },
                LinkedExpression::StructField { left, field_index } => {
                    let struct_value = self.parse_expression(left.expr)?.unwrap().into_pointer_value();
                    let ObjType::Reference(struct_obj_type) = &left.object_type else { unreachable!() };
                    let struct_type = self.parse_type(struct_obj_type);

                    let field_pointer = self.builder.build_struct_gep(struct_type, struct_value, field_index, "get_field")?;
                    Ok(Some(field_pointer.into()))
                }
                LinkedExpression::Literal(literal) => self.parse_literal(literal),
                LinkedExpression::RoundBracket(boxed) => self.parse_expression(boxed.expr),
                LinkedExpression::Variable(object) => {
                    let value_type = self.get_object_type(object);
                    let pointer = self.context_window.get_pointer_unwrap(object);
                    let value = self.builder.build_load(value_type, pointer, "load")?;
                    Ok(Some(value))
                }
                LinkedExpression::UnaryOperation(boxed, op) => {
                    let TypedExpression { expr: linked_expr, object_type } = *boxed;
                    let result = match op {
                        OneSidedOperation::BoolNot => {
                            let exp_parsed = self.parse_expression(linked_expr)?.unwrap();
                            let int_value = exp_parsed.into_int_value();
                            self.builder.build_not(int_value, "not")?.into()
                        }
                        OneSidedOperation::UnaryMinus => {
                            let exp_parsed = self.parse_expression(linked_expr)?.unwrap();
                            let int_value = exp_parsed.into_int_value();
                            self.builder.build_int_neg(int_value, "neg")?.into()
                        }
                        OneSidedOperation::GetReference => {
                            if let Some(ptr_value) = self.get_pointer(&linked_expr)? {
                                ptr_value.into()
                            } else {
                                let exp_parsed = self.parse_expression(linked_expr)?.unwrap();
                                let exp_type = self.parse_type(&object_type);
                                let ptr_value = self.builder.build_alloca(exp_type, "alloca")?;
                                self.builder.build_store(ptr_value, exp_parsed)?;
                                ptr_value.into()
                            }
                        }
                        OneSidedOperation::Dereference => {
                            let exp_parsed = self.parse_expression(linked_expr)?.unwrap();
                            let ptr_value = exp_parsed.into_pointer_value();
                            let ptr_type = self.parse_type(object_type.unwrap_ref());
                            self.builder.build_load(ptr_type, ptr_value, "deref")?
                        }
                    };
                    Ok(Some(result))
                }
                LinkedExpression::Operation(ex1, ex2, op) => {
                    let TypedExpression { expr: linked_expr1, object_type: type1 } = *ex1;
                    let TypedExpression { expr: linked_expr2, object_type: _type2 } = *ex2;

                    let v1 = self.parse_expression(linked_expr1)?.unwrap();
                    let v2 = self.parse_expression(linked_expr2)?.unwrap();
                    Ok(Some(self.parse_operation(v1, v2, type1, op)?))
                }
                LinkedExpression::As(expression, to_obj_type) => {
                    let TypedExpression { expr: linked_expr, object_type: was_obj_type } = *expression;
                    let ex = self.parse_expression(linked_expr)?.unwrap();
                    self.parse_as(ex, was_obj_type, to_obj_type)
                }
            }
        }
        fn parse_literal(&self, literal: LinkedLiteralExpression) -> Result<Option<BasicValueEnum<'ctx>>, LLVMError> {
            match literal {
                LinkedLiteralExpression::Undefined(obj_type) => {
                    let value_type = self.parse_type(&obj_type);
                    Ok(Some(get_undef(value_type)))
                }
                LinkedLiteralExpression::IntLiteral(literal, object_type) => {
                    let int_type = self.parse_type(&object_type).into_int_type();

                    // FIXME: can't parse i128. Uncomment test for i128
                    let value = literal.parse::<u64>().unwrap(); // FIXME: return error
                    let int_value = int_type.const_int(value, true);
                    Ok(Some(int_value.into()))
                }
                LinkedLiteralExpression::FloatLiteral(float, float_type) => {
                    let float_type = self.parse_type(&float_type).into_float_type();

                    let value = float.parse::<f64>().unwrap(); // FIXME: return error
                    Ok(Some(float_type.const_float(value).into()))
                }
                LinkedLiteralExpression::BoolLiteral(bool) => {
                    let int_type = self.context.bool_type();

                    let number = match bool { true => 1, false => 0 };
                    Ok(Some(int_type.const_int(number, false).into()))
                }
                LinkedLiteralExpression::CharLiteral(char) => {
                    let int_type = self.parse_type(&ObjType::Char).into_int_type();
                    Ok(Some(int_type.const_int(char as u64, false).into()))
                }
                LinkedLiteralExpression::StringLiteral(string) => {
                    // SAFETY: looks like [build_global_string] must be called inside function and that's true here
                    let string_value = unsafe { self.builder.build_global_string(&string, "string_literal").unwrap() };
                    Ok(Some(string_value.as_basic_value_enum()))
                }
            }
        }
        fn parse_as(&self, ex: BasicValueEnum<'ctx>, was_obj_type: ObjType, to_obj_type: ObjType) -> Result<Option<BasicValueEnum<'ctx>>, LLVMError> {
            let result = match was_obj_type {
                ObjType::Char => {
                    let ex_int = ex.into_int_value();

                    let from_type = self.parse_type(&ObjType::Char).into_int_type();
                    let to_type = self.parse_type(&to_obj_type).into_int_type();

                    let from_size = from_type.get_bit_width();
                    let to_size = to_type.get_bit_width();

                    match from_size.cmp(&to_size) {
                        Ordering::Equal => {
                            ex
                        }
                        Ordering::Less => { // extend
                            self.builder.build_int_z_extend(ex_int, to_type, "int_casing")?.into()
                        }
                        Ordering::Greater => { // truncation
                            self.builder.build_int_truncate(ex_int, to_type, "int_casing")?.into()
                        }
                    }
                }
                ObjType::Integer(int_type_from) => match to_obj_type {
                    ObjType::Integer(_) | ObjType::Char => {
                        let ex_int = ex.into_int_value();

                        let from_type = self.parse_type(&was_obj_type).into_int_type();
                        let to_type = self.parse_type(&to_obj_type).into_int_type();

                        let from_size = from_type.get_bit_width();
                        let to_size = to_type.get_bit_width();

                        match from_size.cmp(&to_size) {
                            Ordering::Equal => {
                                ex
                            }
                            Ordering::Less => { // extend
                                if int_type_from.is_signed() {
                                    // example: i8 -> i16, i8 -> u16 (i8 -> i16 -> u16)
                                    self.builder.build_int_s_extend(ex_int, to_type, "int_casing")?.into()
                                } else {
                                    // example: u8 -> i16/u16
                                    self.builder.build_int_z_extend(ex_int, to_type, "int_casing")?.into()
                                }
                            }
                            Ordering::Greater => { // truncation
                                self.builder.build_int_truncate(ex_int, to_type, "int_casing")?.into()
                            }
                        }
                    }
                    ObjType::Pointer(reference_type) => {
                        let ex_int = ex.into_int_value();
                        let pointer_type = self.parse_type(&ObjType::Pointer(reference_type)).into_pointer_type();

                        self.builder.build_int_to_ptr(ex_int, pointer_type, "int_to_ptr")?.into()
                    }
                    _ => unreachable!()
                }
                ObjType::Float(_) => {
                    let ex_float = ex.into_float_value();

                    let to_type = self.parse_type(&to_obj_type).into_float_type();

                    let from_size = was_obj_type.get_float_bits_or_panic();
                    let to_size = to_obj_type.get_float_bits_or_panic();

                    match from_size.cmp(&to_size) {
                        Ordering::Equal => {
                            ex
                        }
                        Ordering::Less => { // extend
                            self.builder.build_float_ext(ex_float, to_type, "float_casing")?.into()
                        }
                        Ordering::Greater => { // truncation
                            self.builder.build_float_trunc(ex_float, to_type, "float_casing")?.into()
                        }
                    }
                }
                ObjType::Reference(_) | ObjType::Pointer(_) => {
                    match &to_obj_type {
                        ObjType::Pointer(_) => ex,
                        ObjType::Integer(_) => {
                            let ex_ptr = ex.into_pointer_value();

                            let to_type = self.parse_type(&to_obj_type).into_int_type();

                            self.builder.build_ptr_to_int(ex_ptr, to_type, "asd")?.into()
                        }
                        _ => unreachable!()
                    }
                }
                ObjType::Struct(..) | ObjType::Unknown | ObjType::Void | ObjType::Function { .. } => unreachable!()
            };
            Ok(Some(result))
        }
        fn parse_operation(&self, v1: BasicValueEnum<'ctx>, v2: BasicValueEnum<'ctx>, type1: ObjType, op: TwoSidedOperation) -> Result<BasicValueEnum<'ctx>, LLVMError> {
            let result = match op {
                TwoSidedOperation::Number(num_op) => match type1 {
                    ObjType::Integer(int) => {
                        let num1 = v1.into_int_value();
                        let num2 = v2.into_int_value();
                        match num_op {
                            NumberOperation::Add => self.builder.build_int_add(num1, num2, "add")?.into(),
                            NumberOperation::Sub => self.builder.build_int_sub(num1, num2, "sub")?.into(),
                            NumberOperation::Mul => self.builder.build_int_mul(num1, num2, "mul")?.into(),
                            NumberOperation::Div if int.is_signed() =>  self.builder.build_int_signed_div(num1, num2, "div")?.into(),
                            NumberOperation::Div =>                     self.builder.build_int_unsigned_div(num1, num2, "div")?.into(),
                            NumberOperation::Rem if int.is_signed() =>  self.builder.build_int_signed_rem(num1, num2, "rem")?.into(),
                            NumberOperation::Rem =>                     self.builder.build_int_unsigned_rem(num1, num2, "rem")?.into(),
                            NumberOperation::BitAnd => self.builder.build_and(num1, num2, "bitand")?.into(),
                            NumberOperation::BitOr => self.builder.build_or(num1, num2, "bitor")?.into(),
                        }
                    }
                    ObjType::Float(_) => {
                        let num1 = v1.into_float_value();
                        let num2 = v2.into_float_value();
                        match num_op {
                            NumberOperation::Add => self.builder.build_float_add(num1, num2, "add")?.into(),
                            NumberOperation::Sub => self.builder.build_float_sub(num1, num2, "sub")?.into(),
                            NumberOperation::Mul => self.builder.build_float_mul(num1, num2, "mul")?.into(),
                            NumberOperation::Div => self.builder.build_float_div(num1, num2, "div")?.into(),
                            NumberOperation::Rem | NumberOperation::BitAnd | NumberOperation::BitOr => unreachable!(),
                        }
                    }
                    _ => unreachable!()
                }
                TwoSidedOperation::Bool(bool_op) => {
                    let bool1 = v1.into_int_value();
                    let bool2 = v2.into_int_value();
                    match bool_op {
                        BoolOperation::Or => self.builder.build_or(bool1, bool2, "or")?.into(),
                        BoolOperation::And => self.builder.build_and(bool1, bool2, "and")?.into(),
                    }
                }
                TwoSidedOperation::Compare(comp_op) => match type1 {
                    ObjType::Reference(..) => unreachable!(),
                    ObjType::Pointer(_) => {
                        let ex1 = v1.into_pointer_value();
                        let ex2 = v2.into_pointer_value();
                        let predicate = comp_op.to_int_compare(false);

                        self.builder.build_int_compare(predicate, ex1, ex2, "ptr_compare")?.into()
                    }
                    ObjType::Char => {
                        let ex1 = v1.into_int_value();
                        let ex2 = v2.into_int_value();
                        let predicate = comp_op.to_int_compare(false);
                        self.builder.build_int_compare(predicate, ex1, ex2, "compare_char")?.into()
                    }
                    ObjType::Integer(int) => {
                        let ex1 = v1.into_int_value();
                        let ex2 = v2.into_int_value();
                        let predicate = comp_op.to_int_compare(int.is_signed());
                        self.builder.build_int_compare(predicate, ex1, ex2, "compare_int")?.into()
                    }
                    ObjType::Float(_) => {
                        let ex1 = v1.into_float_value();
                        let ex2 = v2.into_float_value();
                        let predicate = comp_op.to_float_compare();
                        self.builder.build_float_compare(predicate, ex1, ex2, "compare_float")?.into()
                    }
                    ObjType::Struct(..) => unimplemented!(),
                    ObjType::Unknown | ObjType::Void | ObjType::Function { .. } => unreachable!(),
                }
            };
            Ok(result)
        }
        fn call_function(&self, object: Object, args: Vec<TypedExpression>) -> Result<Option<BasicValueEnum<'ctx>>, LLVMError> {
            let args: Vec<_> = args.into_iter().map(|a|
                self.parse_expression(a.expr)
            ).collect::<Result<_, _>>()?;
            let args: Vec<BasicMetadataValueEnum> = args.into_iter().map(|a| a.unwrap().into()).collect(); // will be removed

            let function = self.context_window.get_function_unwrap(object);
            let returned = self.builder.build_call(function, &args, "function call")?;

            Ok(returned.try_as_basic_value().basic())
        }
    }
}

impl CompareOperator {
    const fn to_int_compare(self, signed: bool) -> IntPredicate {
        if signed {
            match self {
                Self::Equal =>           IntPredicate::EQ,
                Self::NotEqual =>        IntPredicate::NE,
                Self::Greater =>         IntPredicate::SGT,
                Self::GreaterEqual =>    IntPredicate::SGE,
                Self::Less =>            IntPredicate::SLT,
                Self::LessEqual =>       IntPredicate::SLE,
            }
        } else {
            match self {
                Self::Equal =>           IntPredicate::EQ,
                Self::NotEqual =>        IntPredicate::NE,
                Self::Greater =>         IntPredicate::UGT,
                Self::GreaterEqual =>    IntPredicate::UGE,
                Self::Less =>            IntPredicate::ULT,
                Self::LessEqual =>       IntPredicate::ULE,
            }
        }
    }

    const fn to_float_compare(self) -> FloatPredicate {
        match self {
            Self::Equal =>           FloatPredicate::OEQ,
            Self::NotEqual =>        FloatPredicate::ONE,
            Self::Greater =>         FloatPredicate::OGT,
            Self::GreaterEqual =>    FloatPredicate::OGE,
            Self::Less =>            FloatPredicate::OLT,
            Self::LessEqual =>       FloatPredicate::OLE,
        }
    }
}
