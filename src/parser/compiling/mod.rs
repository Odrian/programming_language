use std::collections::HashMap;
use std::process::Command;
use std::path::Path;

use inkwell::{context::Context, module::Module, builder::Builder, targets::{InitializationConfig, Target, TargetMachine, FileType, RelocMode, CodeModel}, OptimizationLevel, IntPredicate};
use inkwell::builder::BuilderError;
use inkwell::values::{FunctionValue, IntValue};

use crate::error::CompilationError as CE;
use crate::parser::Config;
use crate::parser::parse4_linking::linked_statement::*;

/// previous steps guarantees that every used variables is valid
pub fn parse_to_llvm(config: &Config, statements: &[LinkedStatement]) -> Result<(), CE> {
    Target::initialize_all(&InitializationConfig::default());
    let context = Context::create();

    let code_module_gen = CodeModuleGen::new(&context, "main_module");
    let result = code_module_gen.parse_statements(statements);
    if let Err(err) = result {
        return Err(CE::LLVMError(err));
    }

    create_executable(config, &code_module_gen.module);
    Ok(())
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
    fn parse_statements(&self, statements: &[LinkedStatement]) -> Result<(), BuilderError> {
        for statement in statements {
            match statement {
                LinkedStatement::Function { object, args, body } => {
                    self.create_function(object, args, body)?;
                }
                _ => {
                    unimplemented!("don't support statements in global")
                }
            }
        }
        Ok(())
    }
    fn create_function(&self, object: &Object, args: &Vec<Object>, body: &Vec<LinkedStatement>) -> Result<(), BuilderError> {
        let i32_type = self.context.i8_type();
        let argument_count = args.len();
        let arguments_types = vec![i32_type.into(); argument_count];
        let fn_type = i32_type.fn_type(&arguments_types, false);

        let function = self.module.add_function(object.get_name().as_str(), fn_type, None);
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        let mut variables_context: HashMap<u32, IntValue> = HashMap::new();
        for (index, object) in args.iter().enumerate() {
            let arg_value = function.get_nth_param(index as u32).unwrap().into_int_value();
            variables_context.insert(object.id, arg_value);
        }

        let function_generator = FunctionGenerator::new(self, function);
        function_generator.parse_function_body(&mut variables_context, body)?;
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
        variables_context: &mut HashMap<u32, IntValue<'ctx>>,
        body: &Vec<LinkedStatement>
    ) -> Result<(), BuilderError> {
        for statement in body {
            self.parse_statement(variables_context, statement)?;
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
        variables_context: &mut HashMap<u32, IntValue<'ctx>>,
        statement: &LinkedStatement
    ) -> Result<(), BuilderError> {
        match statement {
            LinkedStatement::Function { .. } => unimplemented!("local functions are not supported"),
            LinkedStatement::Expression(expression) => {
                self.parse_expression(variables_context, expression)?;
            }
            LinkedStatement::If { condition, body } => {
                let context = self.code_module_gen.context;
                let builder = &self.code_module_gen.builder;

                let condition = self.parse_expression(variables_context, condition)?;
                let zero = context.i32_type().const_int(0, false);
                let condition = builder.build_int_compare(
                    IntPredicate::NE, condition, zero, "if_cond"
                )?;

                let then_bb = context.append_basic_block(self.function, "then");
                let merge_bb = context.append_basic_block(self.function, "merge");
                builder.build_conditional_branch(condition, then_bb, merge_bb)?;

                builder.position_at_end(then_bb);
                for statement in body {
                    self.parse_statement(variables_context, statement)?;
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
                let condition = self.parse_expression(variables_context, condition)?;
                let zero = context.i32_type().const_int(0, false);
                let condition = builder.build_int_compare(
                    IntPredicate::NE, condition, zero, "while_cond"
                )?;
                builder.build_conditional_branch(condition, body_bb, after_bb)?;

                builder.position_at_end(body_bb);
                for statement in body {
                    self.parse_statement(variables_context, statement)?;
                }
                builder.build_unconditional_branch(cond_bb)?;
                builder.position_at_end(after_bb);
            }
            LinkedStatement::SetVariable { object, value } => {
                let expression = self.parse_expression(variables_context, value)?;
                variables_context.insert(object.id, expression);
            }
            LinkedStatement::VariableDeclaration { object, value } => {
                let expression = self.parse_expression(variables_context, value)?;
                variables_context.insert(object.id, expression);
            }
            LinkedStatement::Return(expression) => {
                let expression = self.parse_expression(variables_context, expression)?;
                self.code_module_gen.builder.build_return(Some(&expression))?;
            }
        }
        Ok(())
    }

    fn parse_expression(
        &self,
        variables_context: &mut HashMap<u32, IntValue<'ctx>>,
        expression: &LinkedExpression
    ) -> Result<IntValue<'ctx>, BuilderError> {
        let i32_type = self.code_module_gen.context.i32_type();
        match expression {
            LinkedExpression::FunctionCall { .. } => unimplemented!("function calls not supported"),
            LinkedExpression::NumberLiteral(literal) => {
                let number = literal.iter().collect::<String>().parse::<i32>().unwrap();
                Ok(i32_type.const_int(number as u64, false))
            }
            LinkedExpression::RoundBracket(boxed) => self.parse_expression(variables_context, boxed),
            LinkedExpression::Variable(object) => {
                Ok(*variables_context.get(&object.id).unwrap())
            }
            LinkedExpression::Plus(ex1, ex2) => {
                let ex1 = self.parse_expression(variables_context, ex1)?;
                let ex2 = self.parse_expression(variables_context, ex2)?;
                self.code_module_gen.builder.build_int_add(ex1, ex2, "sum")
            }
        }
    }
}

impl Object<'_> {
    fn get_name(&self) -> String {
        let name = self.name.iter().collect::<String>();
        if self.name_id == 0 {
            name
        } else {
            format!("{}.{}", name, self.name_id)
        }
    }
}

fn create_executable(config: &Config, module: &Module) {
    let assembly_name = format!("{}.ll", config.output);
    let object_name = format!("{}.o", config.output);
    let executable_name = config.output.clone();

    // create assembly file
    if config.create_llvm_ir {
        module.print_to_file(assembly_name)
            .unwrap_or_else(|err| panic!("failed to dump LLVM IR: {err}"));
    }

    if !config.create_object && !config.create_executable {
        return
    }

    let tm = create_target_machine();
    module.set_triple(&tm.get_triple());

    // create object file
    tm.write_to_file(module, FileType::Object, Path::new(object_name.as_str()))
        .unwrap_or_else(|err| panic!("failed to create object file: {err}"));

    if config.create_executable {
        let status = Command::new("cc")
            .args([object_name.as_str(), "-o", executable_name.as_str()])
            .status()
            .unwrap_or_else(|err| panic!("Failed to invoke linker 'cc': {err}"));
        if !status.success() {
            panic!("Linker exited with {:?} code", status.code());
        }
    }
    if !config.create_object {
        std::fs::remove_file(object_name)
            .unwrap_or_else(|err| panic!("failed to delete object file: {err}"));
    }
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
