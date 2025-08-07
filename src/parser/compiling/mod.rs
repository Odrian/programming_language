mod context_window;
mod module_generator;

use std::process::Command;
use std::path::Path;

use inkwell::{context::Context, module::Module, targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine}, OptimizationLevel};

use crate::error::CompilationError as CE;
use crate::parser::Config;
use crate::parser::parse3_linking::linked_statement::*;
use crate::parser::parse3_linking::object::ObjectFactory;

/// previous steps guarantees that every used variables is valid
pub fn parse_to_llvm(config: &Config, statements: &[LinkedStatement], object_factory: &ObjectFactory) -> Result<(), CE> {
    Target::initialize_all(&InitializationConfig::default());
    let context = Context::create();

    let module = module_generator::parse_module(&context, statements, object_factory)?;

    if let Err(err) = module.verify() {
        return Err(CE::LLVMVerifyModuleError { llvm_error: err.to_string() });
    }
    
    if !verify_main_exist(statements) {
        return Err(CE::NoMainFunction)
    }

    create_executable(config, &module)?;
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
    tm.write_to_file(module, FileType::Object, Path::new(object_name.as_str()))
        .expect("functions and whole module was verified, shouldn't return error");

    let status_option = if config.create_executable {
        let status = Command::new("cc")
            .args([object_name.as_str(), "-o", executable_name.as_str()]).status();
        Some(status)
    } else { None };

    if !config.create_object {
        if let Err(err) = std::fs::remove_file(object_name.clone()) {
            return Err(CE::CantDeleteObjectFile { filepath: object_name, io_error: err.to_string() });
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
