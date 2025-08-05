mod context_window;

use context_window::ValueContextWindow;

use std::process::Command;
use std::path::Path;

use inkwell::{context::Context, module::Module, builder::Builder, targets::{InitializationConfig, Target, TargetMachine, FileType, RelocMode, CodeModel}, OptimizationLevel, IntPredicate};
use inkwell::values::{BasicMetadataValueEnum, FunctionValue, IntValue};

use crate::error::CompilationError as CE;
use crate::parser::Config;
use crate::parser::parse1_tokenize::token::TwoSidedOperation;
use crate::parser::parse3_linking::linked_statement::*;

// TODO: split module to submodules

/// previous steps guarantees that every used variables is valid
pub fn parse_to_llvm(config: &Config, statements: &[LinkedStatement]) -> Result<(), CE> {
    Target::initialize_all(&InitializationConfig::default());
    let context = Context::create();

    let code_module_gen = CodeModuleGen::new(&context, "main_module");
    code_module_gen.parse_statements(statements)?;
    
    if let Err(err) = code_module_gen.module.verify() {
        return Err(CE::LLVMVerifyModuleError { llvm_error: err.to_string() });
    }
    
    if !verify_main_exist(statements) {
        return Err(CE::NoMainFunction)
    }

    create_executable(config, &code_module_gen.module)?;
    Ok(())
}

fn verify_main_exist(statements: &[LinkedStatement]) -> bool {
    for statement in statements {
        if let LinkedStatement::Function { object, .. } = statement {
            if object.name == ['m', 'a', 'i', 'n'] {
                // TODO: check that main has correct signature
                return true
            }
        }
    }
    false
}

struct CodeModuleGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
}
impl<'ctx> CodeModuleGen<'ctx> {
    fn new(context: &'ctx Context, name: &str) -> Self {
        let module = context.create_module(name);
        let builder = context.create_builder();
        Self {
            context,
            module,
            builder
        }
    }
    fn parse_statements(&self, statements: &[LinkedStatement]) -> Result<(), CE> {
        let mut context_window = ValueContextWindow::new();
        context_window.step_in();
        for statement in statements {
            match statement {
                LinkedStatement::Function { .. } => {
                    self.create_function(&mut context_window, statement)?;
                }
                _ => {
                    unimplemented!("don't support statements in global")
                }
            }
        }
        context_window.step_out();
        Ok(())
    }
    fn create_function(&self, context_window: &mut ValueContextWindow<'ctx>, statement: &LinkedStatement) -> Result<(), CE> {
        let LinkedStatement::Function { object, args, body } = statement else { unreachable!() };

        let i32_type = self.context.i32_type();
        let argument_count = args.len();
        let arguments_types = vec![i32_type.into(); argument_count];
        let fn_type = i32_type.fn_type(&arguments_types, false);

        let function = self.module.add_function(object.get_name().as_str(), fn_type, None);
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        context_window.add(object, function.into());
        context_window.step_in();

        for (index, object) in args.iter().enumerate() {
            let arg_value = function.get_nth_param(index as u32).unwrap().into_int_value();
            let pointer = self.builder.build_alloca(i32_type, &format!("arg{index}"))?;
            self.builder.build_store(pointer, arg_value)?;
            context_window.add(object, pointer.into());
        }

        let function_generator = FunctionGenerator::new(self, function);
        function_generator.parse_function_body(context_window, body)?;
        if !function.verify(true) {
            return Err(CE::LLVMVerifyFunctionError { name: object.get_name() });
        }

        context_window.step_out();
        Ok(())
    }
}

struct FunctionGenerator<'ctx, 'a> {
    code_module_gen: &'a CodeModuleGen<'ctx>,
    function: FunctionValue<'ctx>,
}
impl<'ctx, 'a> FunctionGenerator<'ctx, 'a> {
    fn new(code_module_gen: &'a CodeModuleGen<'ctx>, function: FunctionValue<'ctx>, ) -> Self {
        Self {
            code_module_gen,
            function,
        }
    }

    fn parse_function_body(
        &self,
        context_window: &mut ValueContextWindow<'ctx>,
        body: &Vec<LinkedStatement>
    ) -> Result<(), CE> {
        for statement in body {
            self.parse_statement(context_window, statement)?;
        }

        // TODO: check that function always return something
        // TODO: add return zero by default from main function
        // let i32_type = self.code_module_gen.context.i32_type();
        // let zero = i32_type.const_int(0, false);
        // self.code_module_gen.builder.build_return(Some(&zero))?;
        Ok(())
    }

    fn parse_statement(
        &self,
        context_window: &mut ValueContextWindow<'ctx>,
        statement: &LinkedStatement
    ) -> Result<(), CE> {
        match statement {
            LinkedStatement::Function { .. } => unimplemented!("local functions are not supported"),
            LinkedStatement::Expression(expression) => {
                self.parse_expression(context_window, expression)?;
            }
            LinkedStatement::If { condition, body } => {
                let context = self.code_module_gen.context;
                let builder = &self.code_module_gen.builder;

                let condition = self.parse_expression(context_window, condition)?;
                let zero = context.i32_type().const_int(0, false);
                let condition = builder.build_int_compare(
                    IntPredicate::NE, condition, zero, "if_cond"
                )?;

                let then_bb = context.append_basic_block(self.function, "then");
                let merge_bb = context.append_basic_block(self.function, "merge");
                builder.build_conditional_branch(condition, then_bb, merge_bb)?;

                builder.position_at_end(then_bb);
                for statement in body {
                    self.parse_statement(context_window, statement)?;
                }
                builder.build_unconditional_branch(merge_bb)?;
                builder.position_at_end(merge_bb);
            }
            LinkedStatement::While { condition, body } => {
                let context = self.code_module_gen.context;
                let builder = &self.code_module_gen.builder;

                let cond_bb = context.append_basic_block(self.function, "cond");
                let body_bb = context.append_basic_block(self.function, "then");
                let after_bb = context.append_basic_block(self.function, "merge");

                builder.build_unconditional_branch(cond_bb)?;

                builder.position_at_end(cond_bb);
                let condition = self.parse_expression(context_window, condition)?;
                let zero = context.i32_type().const_int(0, false);
                let condition = builder.build_int_compare(
                    IntPredicate::NE, condition, zero, "while_cond"
                )?;
                builder.build_conditional_branch(condition, body_bb, after_bb)?;

                builder.position_at_end(body_bb);
                for statement in body {
                    self.parse_statement(context_window, statement)?;
                }
                builder.build_unconditional_branch(cond_bb)?;
                builder.position_at_end(after_bb);
            }
            LinkedStatement::SetVariable { object, value } => {
                let builder = &self.code_module_gen.builder;

                let value = self.parse_expression(context_window, value)?;
                let pointer = context_window.get_pointer_unwrap(object);
                builder.build_store(pointer, value)?;
            }
            LinkedStatement::VariableDeclaration { object, value } => {
                let context = self.code_module_gen.context;
                let builder = &self.code_module_gen.builder;

                let value = self.parse_expression(context_window, value)?;
                let var_type = context.i32_type();
                let pointer = builder.build_alloca(var_type, &object.get_name())?;
                builder.build_store(pointer, value)?;
                context_window.add(object, pointer.into());
            }
            LinkedStatement::Return(expression) => {
                let expression = self.parse_expression(context_window, expression)?;
                self.code_module_gen.builder.build_return(Some(&expression))?;
            }
        }
        Ok(())
    }

    fn parse_expression(
        &self,
        context_window: &mut ValueContextWindow<'ctx>,
        expression: &LinkedExpression
    ) -> Result<IntValue<'ctx>, CE> {
        let i32_type = self.code_module_gen.context.i32_type();
        match expression {
            LinkedExpression::FunctionCall { object, args } => {
                let args: Vec<_> = args.iter().map(|a|
                    self.parse_expression(context_window, a)
                ).collect::<Result<_, _>>()?;
                let args: Vec<BasicMetadataValueEnum> = args.into_iter().map(|a| a.into()).collect(); // will be removed

                let function = context_window.get_function_unwrap(object);
                let returned = self.code_module_gen.builder.build_call(function, &args, "function call")?;
                // now all functions return i32
                let returned_value = returned.try_as_basic_value().unwrap_left();
                Ok(returned_value.into_int_value())
            },
            LinkedExpression::NumberLiteral(literal) => {
                let number = literal.iter().collect::<String>().parse::<i32>().unwrap();
                Ok(i32_type.const_int(number as u64, false))
            }
            LinkedExpression::RoundBracket(boxed) => self.parse_expression(context_window, boxed),
            LinkedExpression::Variable(object) => {
                let value_type = self.code_module_gen.context.i32_type();
                let pointer = context_window.get_pointer_unwrap(object);
                let value = self.code_module_gen.builder.build_load(value_type, pointer, "load")?;
                Ok(value.into_int_value())
            }
            LinkedExpression::TwoSidedOp(ex1, ex2, op) => {
                let ex1 = self.parse_expression(context_window, ex1)?;
                let ex2 = self.parse_expression(context_window, ex2)?;
                match op {
                    TwoSidedOperation::Plus => {
                        let result = self.code_module_gen.builder.build_int_add(ex1, ex2, "sum")?;
                        Ok(result)
                    }
                    TwoSidedOperation::Minus => {
                        let result = self.code_module_gen.builder.build_int_sub(ex1, ex2, "dif")?;
                        Ok(result)
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

fn create_executable(config: &Config, module: &Module) -> Result<(), CE> {
    let assembly_name = format!("{}.ll", config.output);
    let object_name = format!("{}.o", config.output);
    let executable_name = config.output.clone();

    // create assembly file
    if config.create_llvm_ir {
        if let Err(err) = module.print_to_file(assembly_name) {
            return Err(CE::LLVMFailedToCreateAssembly { llvm_error: err.to_string() });
        }
    }

    if !config.create_object && !config.create_executable {
        return Ok(())
    }

    let tm = create_target_machine();
    module.set_triple(&tm.get_triple());

    // create object file
    if let Err(err) = tm.write_to_file(module, FileType::Object, Path::new(object_name.as_str())) {
        panic!("functions and whole module was verified, but got error: {}", err.to_string());
    }

    let status_option = if config.create_executable {
        let status = Command::new("cc")
            .args([object_name.as_str(), "-o", executable_name.as_str()]).status();
        Some(status)
    } else { None };

    if !config.create_object {
        if let Err(err) = std::fs::remove_file(object_name.clone()) {
            return Err(CE::FailedToDeleteObject { name: object_name, io_err: err.to_string() });
        }
    }

    if let Some(status) = status_option {
        // parse linking result
        match status {
            Ok(exit_status) => {
                if !exit_status.success() {
                    let description = format!("exit with code {}", exit_status.code().unwrap());
                    return Err(CE::FailedToRunLinker { description })
                }
            }
            Err(err) => {
                return Err(CE::FailedToRunLinker { description: err.to_string() })
            }
        }
    }
    Ok(())
}

fn create_target_machine() -> TargetMachine {
    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple)
        .expect("Failed to get target from default triple");
    let cpu = "generic";
    let features = "";

    target.create_target_machine(
        &triple,
        cpu,
        features,
        OptimizationLevel::None,
        RelocMode::Default,
        CodeModel::Default,
    ).expect("Could not create TargetMachine")
}
