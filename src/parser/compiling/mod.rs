mod context_window;
mod module_generator;
mod error;

use std::process::Command;
use std::path::Path;

use inkwell::{context::Context, module::Module, targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine}, OptimizationLevel};

use crate::Args;
use error::LLVMError;
use crate::parser::parse3_linking::linked_statement::GlobalLinkedStatement;
use crate::parser::parse3_linking::LinkedProgram;
use crate::parser::parse3_linking::object::{IntObjType, ObjType};

/// previous steps guarantees that every used variables is valid
pub fn parse_to_llvm(args: &Args, linked_program: LinkedProgram) -> Result<(), LLVMError> {
    Target::initialize_all(&InitializationConfig::default());
    let context = Context::create();

    verify_main_signature(&linked_program)?;

    let target_machine = create_target_machine(args);
    let target_data = target_machine.get_target_data();

    let module = module_generator::parse_module(&context, &target_data, linked_program)?;
    module.set_triple(&target_machine.get_triple());

    if let Err(err) = module.verify() {
        return Err(LLVMError::LLVMVerifyModuleError { llvm_error: err.to_string() });
    }

    create_executable(args, &target_machine, &module)?;
    Ok(())
}

fn verify_main_signature(linked_program: &LinkedProgram) -> Result<(), LLVMError> {
    for (object, statement) in &linked_program.function_statement {
        if let GlobalLinkedStatement::Function { returns, args, body: _body } = statement {
            if linked_program.factory.get_name(*object) == "main" {
                if returns != &ObjType::Integer(IntObjType::I32) {
                    return Err(LLVMError::IncorrectMainSignature)
                }
                if args != &vec![] {
                    return Err(LLVMError::IncorrectMainSignature)
                }
                return Ok(())
            }
        }
    }
    Err(LLVMError::NoMainFunction)
}

fn create_executable(args: &Args, tm: &TargetMachine, module: &Module) -> Result<(), LLVMError> {
    let assembly_name = format!("{}.ll", args.output);
    let object_name = format!("{}.o", args.output);
    let executable_name = args.output.clone();

    // create assembly file
    if args.create_llvm_ir {
        if let Err(err) = module.print_to_file(assembly_name) {
            return Err(LLVMError::LLVMFailedToCreateAssembly { llvm_error: err.to_string() });
        }
    }

    if !args.create_object && args.dont_create_executable {
        return Ok(())
    }

    // create object file
    if let Err(err) = tm.write_to_file(module, FileType::Object, Path::new(object_name.as_str())) {
        panic!("functions and whole module was verified, but got llvm error: {err}")
    }

    let status_option = if !args.dont_create_executable {
        let status = Command::new("cc")
            .args([object_name.as_str(), "-o", executable_name.as_str()]).status();
        Some(status)
    } else { None };

    if !args.create_object {
        if let Err(err) = std::fs::remove_file(object_name.clone()) {
            return Err(LLVMError::CantDeleteObjectFile { filepath: object_name, io_error: err.to_string() });
        }
    }

    if let Some(status) = status_option {
        // parse linking result
        match status {
            Ok(exit_status) => {
                if !exit_status.success() {
                    let description = format!("exit with code {}", exit_status.code().unwrap());
                    return Err(LLVMError::FailedToRunLinker { description })
                }
            }
            Err(err) => {
                return Err(LLVMError::FailedToRunLinker { description: err.to_string() })
            }
        }
    }
    Ok(())
}

fn create_target_machine(_args: &Args) -> TargetMachine {
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
