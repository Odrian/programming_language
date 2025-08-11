use crate::error::CompilationError as CE;
use crate::parser::operations::*;
use crate::parser::parse3_linking::linked_statement::*;
use super::context_window::ValueContextWindow;

use inkwell::values::{BasicValueEnum, BasicMetadataValueEnum, FunctionValue};
use inkwell::{builder::Builder, context::Context, module::Module, IntPredicate};
use inkwell::types::AnyTypeEnum;
use crate::parser::parse3_linking::object::*;

pub fn parse_module<'ctx>(context: &'ctx Context, statements: Vec<LinkedStatement>, object_factory: &ObjectFactory) -> Result<Module<'ctx>, CE> {
    let mut code_module_gen = CodeModuleGen::new(context, object_factory, "main_module");
    code_module_gen.parse_module(statements)?;
    Ok(code_module_gen.module)
}

struct CodeModuleGen<'ctx, 'factory> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    object_factory: &'factory ObjectFactory,
    current_function: Option<FunctionValue<'ctx>>,
    context_window: ValueContextWindow<'ctx>,
}
impl<'ctx, 'factory> CodeModuleGen<'ctx, 'factory> {
    fn new(context: &'ctx Context, object_factory: &'factory ObjectFactory, name: &str) -> Self {
        let module = context.create_module(name);
        let builder = context.create_builder();
        let context_window = ValueContextWindow::new();
        Self {
            context,
            module,
            builder,
            object_factory,
            current_function: None,
            context_window
        }
    }
    fn get_object_name(&self, object: Object) -> &String {
        self.object_factory.get_name(object)
    }
    fn get_object_type(&self, object: Object) -> Option<AnyTypeEnum> {
        self.parse_type(self.object_factory.get_type(object))
    }
    fn parse_type(&self, typee: &ObjType) -> Option<AnyTypeEnum> {
        match typee {
            ObjType::Unit => None,
            ObjType::Bool => Some(self.context.bool_type().into()),
            ObjType::Number => Some(self.context.i32_type().into()),
            ObjType::Function { .. } => unimplemented!(),
        }
    }
}

/// parsing statements in global space
mod module_parsing {
    use super::*;

    impl<'ctx> CodeModuleGen<'ctx, '_> {
        pub fn parse_module(&mut self, statements: Vec<LinkedStatement>) -> Result<(), CE> {
            self.context_window.step_in();
            for statement in statements {
                match statement {
                    LinkedStatement::Function { .. } => {
                        self.create_function(statement)?;
                    }
                    _ => {
                        return Err(CE::UnexpectedGlobalVariable)
                    }
                }
            }
            self.context_window.step_out();
            Ok(())
        }

        fn create_function(&mut self, statement: LinkedStatement) -> Result<(), CE> {
            let LinkedStatement::Function { object, args, returns: _, body } = statement else { unreachable!() };

            let i32_type = self.context.i32_type();
            let argument_count = args.len();
            let arguments_types = vec![i32_type.into(); argument_count];
            let fn_type = i32_type.fn_type(&arguments_types, false);

            let function = self.module.add_function(self.get_object_name(object).as_str(), fn_type, None);
            let entry = self.context.append_basic_block(function, "entry");
            self.builder.position_at_end(entry);

            self.context_window.add(object, function.into());
            self.context_window.step_in();

            for (index, object) in args.into_iter().enumerate() {
                let arg_value = function.get_nth_param(index as u32).unwrap().into_int_value();
                let pointer = self.builder.build_alloca(i32_type, &format!("arg{index}"))?;
                self.builder.build_store(pointer, arg_value)?;
                self.context_window.add(object, pointer.into());
            }

            self.current_function = Some(function);
            self.parse_function_body(body)?;
            if !function.verify(true) {
                return Err(CE::LLVMVerifyFunctionError { name: self.get_object_name(object).clone() });
            }
            self.current_function = None;

            self.context_window.step_out();
            Ok(())
        }
    }
}

/// parsing statements inside function
mod function_parsing {
    use super::*;

    impl<'ctx> CodeModuleGen<'ctx, '_> {
        pub fn parse_function_body(&mut self, body: Vec<LinkedStatement>) -> Result<(), CE> {
            let is_returned = self.parse_statements(body)?;
            if !is_returned {
                self.builder.build_return(None)?;
            }

            // TODO: don't require 'return' nothing and 'return 0' in 'main'
            // let i32_type = self.code_module_gen.context.i32_type();
            // let zero = i32_type.const_int(0, false);
            // self.code_module_gen.builder.build_return(Some(&zero))?;
            Ok(())
        }
        fn parse_statements(&mut self, statements: Vec<LinkedStatement>) -> Result<bool, CE> {
            for statement in statements {
                if self.parse_statement(statement)? {
                    return Ok(true)
                }
            }
            Ok(false)
        }

        fn parse_statement(&mut self, statement: LinkedStatement) -> Result<bool, CE> {
            match statement {
                LinkedStatement::Function { .. } => unimplemented!("local functions are not supported"),
                LinkedStatement::Expression(expression) => {
                    self.parse_expression(expression)?;
                }
                LinkedStatement::If { condition, body } => {
                    let condition_value = self.parse_expression(condition)?.into_int_value();

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
                    let condition_value = self.parse_expression(condition)?.into_int_value();
                    self.builder.build_conditional_branch(condition_value, body_bb, after_bb)?;

                    self.builder.position_at_end(body_bb);
                    let has_return = self.parse_statements(body)?;
                    if has_return {
                        return Ok(true)
                    }
                    self.builder.build_unconditional_branch(cond_bb)?;
                    self.builder.position_at_end(after_bb);
                }
                LinkedStatement::SetVariable { object, value } => {
                    let value = self.parse_expression(value)?;
                    let pointer = self.context_window.get_pointer_unwrap(object);
                    self.builder.build_store(pointer, value)?;
                }
                LinkedStatement::VariableDeclaration { object, value } => {
                    let value = self.parse_expression(value)?;
                    let var_type = self.context.i32_type();
                    let pointer = self.builder.build_alloca(var_type, self.get_object_name(object))?;
                    self.builder.build_store(pointer, value)?;
                    self.context_window.add(object, pointer.into());
                }
                LinkedStatement::Return(expression) => {
                    if let Some(expression) = expression {
                        let expression = self.parse_expression(expression)?;
                        self.builder.build_return(Some(&expression))?;
                    } else {
                        self.builder.build_return(None)?;
                    }
                    return Ok(true)
                }
            }
            Ok(false)
        }

        fn parse_expression(&self, expression: TypedExpression) -> Result<BasicValueEnum<'ctx>, CE> {
            match expression.expr {
                LinkedExpression::FunctionCall { object, args } => {
                    let args: Vec<_> = args.into_iter().map(|a|
                        self.parse_expression(a)
                    ).collect::<Result<_, _>>()?;
                    let args: Vec<BasicMetadataValueEnum> = args.into_iter().map(|a| a.into()).collect(); // will be removed

                    let function = self.context_window.get_function_unwrap(object);
                    let returned = self.builder.build_call(function, &args, "function call")?;
                    // now all functions return i32
                    let returned_value = returned.try_as_basic_value().unwrap_left();
                    Ok(returned_value)
                },
                LinkedExpression::NumberLiteral(literal) => {
                    let number = literal.parse::<i32>().unwrap();
                    Ok(self.context.i32_type().const_int(number as u64, false).into())
                }
                LinkedExpression::BoolLiteral(value) => {
                    let number = match value { true => 1, false => 0 };
                    Ok(self.context.bool_type().const_int(number, false).into())
                }
                LinkedExpression::RoundBracket(boxed) => self.parse_expression(*boxed),
                LinkedExpression::Variable(object) => {
                    let value_type = self.context.i32_type();
                    let pointer = self.context_window.get_pointer_unwrap(object);
                    let value = self.builder.build_load(value_type, pointer, "load")?;
                    Ok(value)
                }
                LinkedExpression::UnaryOperation(boxed, op) => {
                    let exp_parsed = self.parse_expression(*boxed)?;
                    match op {
                        OneSidedOperation::BoolNot => {
                            let num = exp_parsed.into_int_value();
                            Ok(self.builder.build_not(num, "not")?.into())
                        }
                        OneSidedOperation::UnaryMinus => {
                            let num = exp_parsed.into_int_value();
                            Ok(self.builder.build_int_neg(num, "neg")?.into())
                        }
                    }
                }
                LinkedExpression::Operation(ex1, ex2, op) => {
                    let type1 = ex1.typee.clone();
                    let ex1_parsed = self.parse_expression(*ex1)?;
                    let ex2_parsed = self.parse_expression(*ex2)?;
                    match op {
                        TwoSidedOperation::Number(num_op) => {
                            let num1 = ex1_parsed.into_int_value();
                            let num2 = ex2_parsed.into_int_value();
                            match num_op {
                                NumberOperation::Add => Ok(self.builder.build_int_add(num1, num2, "add")?.into()),
                                NumberOperation::Sub => Ok(self.builder.build_int_sub(num1, num2, "sub")?.into()),
                                NumberOperation::Mul => Ok(self.builder.build_int_mul(num1, num2, "mul")?.into()),
                                NumberOperation::Div => Ok(self.builder.build_int_signed_div(num1, num2, "div")?.into()),
                                NumberOperation::Rem => Ok(self.builder.build_int_signed_rem(num1, num2, "rem")?.into()),
                                NumberOperation::BitAnd => Ok(self.builder.build_and(num1, num2, "bitand")?.into()),
                                NumberOperation::BitOr => Ok(self.builder.build_or(num1, num2, "bitor")?.into()),
                            }
                        }
                        TwoSidedOperation::Bool(bool_op) => {
                            let bool1 = ex1_parsed.into_int_value();
                            let bool2 = ex2_parsed.into_int_value();
                            match bool_op {
                                BoolOperation::Or => Ok(self.builder.build_or(bool1, bool2, "or")?.into()),
                                BoolOperation::And => Ok(self.builder.build_and(bool1, bool2, "and")?.into()),
                            }
                        }
                        TwoSidedOperation::Compare(comp_op) => match type1 {
                            ObjType::Number | ObjType::Bool => {
                                let ex1 = ex1_parsed.into_int_value();
                                let ex2 = ex2_parsed.into_int_value();
                                let predicate = comp_op.to_int_compare();
                                Ok(self.builder.build_int_compare(predicate, ex1, ex2, "equals")?.into())
                            }
                            ObjType::Unit | ObjType::Function { .. } => unimplemented!(),
                        }
                    }
                }
            }
        }
    }
}


impl CompareOperator {
    fn to_int_compare(self) -> IntPredicate {
        match self {
            CompareOperator::Equal =>           IntPredicate::EQ,
            CompareOperator::NotEqual =>        IntPredicate::NE,
            CompareOperator::Greater =>         IntPredicate::SGT,
            CompareOperator::GreaterEqual =>    IntPredicate::SGE,
            CompareOperator::Less =>            IntPredicate::SLT,
            CompareOperator::LessEqual =>       IntPredicate::SLE,
        }
    }

    fn to_uint_compare(self) -> IntPredicate {
        match self {
            CompareOperator::Equal =>           IntPredicate::EQ,
            CompareOperator::NotEqual =>        IntPredicate::NE,
            CompareOperator::Greater =>         IntPredicate::UGT,
            CompareOperator::GreaterEqual =>    IntPredicate::UGE,
            CompareOperator::Less =>            IntPredicate::ULT,
            CompareOperator::LessEqual =>       IntPredicate::ULE,
        }
    }
}
