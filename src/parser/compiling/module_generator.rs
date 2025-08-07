use crate::error::CompilationError as CE;
use crate::parser::parse1_tokenize::token::TwoSidedOperation;
use crate::parser::parse3_linking::linked_statement::*;
use super::context_window::ValueContextWindow;

use inkwell::values::{BasicMetadataValueEnum, FunctionValue, IntValue};
use inkwell::{builder::Builder, context::Context, module::Module, IntPredicate};
use crate::parser::parse3_linking::object::*;

pub fn parse_module<'ctx>(context: &'ctx Context, statements: &[LinkedStatement], object_factory: &ObjectFactory) -> Result<Module<'ctx>, CE> {
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
}

/// parsing statements in global space
mod module_parsing {
    use super::*;

    impl<'ctx> CodeModuleGen<'ctx, '_> {
        pub fn parse_module(&mut self, statements: &[LinkedStatement]) -> Result<(), CE> {
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

        fn create_function(&mut self, statement: &LinkedStatement) -> Result<(), CE> {
            let LinkedStatement::Function { object, args, returns, body } = statement else { unreachable!() };

            let i32_type = self.context.i32_type();
            let argument_count = args.len();
            let arguments_types = vec![i32_type.into(); argument_count];
            let fn_type = i32_type.fn_type(&arguments_types, false);

            let function = self.module.add_function(object.get_name().as_str(), fn_type, None);
            let entry = self.context.append_basic_block(function, "entry");
            self.builder.position_at_end(entry);

            self.context_window.add(object, function.into());
            self.context_window.step_in();

            for (index, object) in args.iter().enumerate() {
                let arg_value = function.get_nth_param(index as u32).unwrap().into_int_value();
                let pointer = self.builder.build_alloca(i32_type, &format!("arg{index}"))?;
                self.builder.build_store(pointer, arg_value)?;
                self.context_window.add(object, pointer.into());
            }

            self.current_function = Some(function);
            self.parse_function_body(body)?;
            if !function.verify(true) {
                return Err(CE::LLVMVerifyFunctionError { name: object.get_name() });
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
        pub fn parse_function_body(&mut self, body: &Vec<LinkedStatement>) -> Result<(), CE> {
            for statement in body {
                self.parse_statement(statement)?;
            }

            // TODO: check that function always return something
            // TODO: add return zero by default from main function
            // let i32_type = self.code_module_gen.context.i32_type();
            // let zero = i32_type.const_int(0, false);
            // self.code_module_gen.builder.build_return(Some(&zero))?;
            Ok(())
        }

        fn parse_statement(&mut self, statement: &LinkedStatement) -> Result<(), CE> {
            match statement {
                LinkedStatement::Function { .. } => unimplemented!("local functions are not supported"),
                LinkedStatement::Expression(expression) => {
                    self.parse_expression(expression)?;
                }
                LinkedStatement::If { condition, body } => {
                    let condition = self.parse_expression(condition)?;
                    let zero = self.context.i32_type().const_int(0, false);
                    let condition = self.builder.build_int_compare(
                        IntPredicate::NE, condition, zero, "if_cond"
                    )?;

                    let function = self.current_function.unwrap();
                    let then_bb = self.context.append_basic_block(function, "then");
                    let merge_bb = self.context.append_basic_block(function, "merge");
                    self.builder.build_conditional_branch(condition, then_bb, merge_bb)?;

                    self.builder.position_at_end(then_bb);
                    for statement in body {
                        self.parse_statement(statement)?;
                    }
                    self.builder.build_unconditional_branch(merge_bb)?;
                    self.builder.position_at_end(merge_bb);
                }
                LinkedStatement::While { condition, body } => {
                    let function = self.current_function.unwrap();
                    let cond_bb = self.context.append_basic_block(function, "cond");
                    let body_bb = self.context.append_basic_block(function, "then");
                    let after_bb = self.context.append_basic_block(function, "merge");

                    self.builder.build_unconditional_branch(cond_bb)?;

                    self.builder.position_at_end(cond_bb);
                    let condition = self.parse_expression(condition)?;
                    let zero = self.context.i32_type().const_int(0, false);
                    let condition = self.builder.build_int_compare(
                        IntPredicate::NE, condition, zero, "while_cond"
                    )?;
                    self.builder.build_conditional_branch(condition, body_bb, after_bb)?;

                    self.builder.position_at_end(body_bb);
                    for statement in body {
                        self.parse_statement(statement)?;
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
                    let pointer = self.builder.build_alloca(var_type, &object.get_name())?;
                    self.builder.build_store(pointer, value)?;
                    self.context_window.add(object, pointer.into());
                }
                LinkedStatement::Return(expression) => {
                    let expression = self.parse_expression(expression)?;
                    self.builder.build_return(Some(&expression))?;
                }
            }
            Ok(())
        }

        fn parse_expression(&self, expression: &TypedExpression) -> Result<IntValue<'ctx>, CE> {
            let i32_type = self.context.i32_type();
            match &expression.expr {
                LinkedExpression::FunctionCall { object, args } => {
                    let args: Vec<_> = args.iter().map(|a|
                        self.parse_expression(a)
                    ).collect::<Result<_, _>>()?;
                    let args: Vec<BasicMetadataValueEnum> = args.into_iter().map(|a| a.into()).collect(); // will be removed

                    let function = self.context_window.get_function_unwrap(object);
                    let returned = self.builder.build_call(function, &args, "function call")?;
                    // now all functions return i32
                    let returned_value = returned.try_as_basic_value().unwrap_left();
                    Ok(returned_value.into_int_value())
                },
                LinkedExpression::NumberLiteral(literal) => {
                    let number = literal.iter().collect::<String>().parse::<i32>().unwrap();
                    Ok(i32_type.const_int(number as u64, false))
                }
                LinkedExpression::RoundBracket(boxed) => self.parse_expression(boxed),
                LinkedExpression::Variable(object) => {
                    let value_type = self.context.i32_type();
                    let pointer = self.context_window.get_pointer_unwrap(object);
                    let value = self.builder.build_load(value_type, pointer, "load")?;
                    Ok(value.into_int_value())
                }
                LinkedExpression::TwoSidedOp(ex1, ex2, op) => {
                    let ex1 = self.parse_expression(ex1)?;
                    let ex2 = self.parse_expression(ex2)?;
                    match op {
                        TwoSidedOperation::Plus => {
                            let result = self.builder.build_int_add(ex1, ex2, "sum")?;
                            Ok(result)
                        }
                        TwoSidedOperation::Minus => {
                            let result = self.builder.build_int_sub(ex1, ex2, "dif")?;
                            Ok(result)
                        }
                    }
                }
            }
        }
    }
}

impl Object<'_> {
    fn get_name(&self) -> String {
        let name = self.name.iter().collect::<String>();
        if name == "main" {
            name
        } else {
            format!("{}.{}", name, self.id)
        }
    }
}
