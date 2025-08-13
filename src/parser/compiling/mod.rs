mod context_window;
mod module_generator;

use std::process::Command;
use std::path::Path;

use inkwell::{context::Context, module::Module, targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine}, OptimizationLevel};

use crate::error::CompilationError as CE;
use crate::Config;
use crate::parser::parse3_linking::linked_statement::*;
use crate::parser::parse3_linking::object::{IntObjType, ObjType, ObjectFactory};

/// previous steps guarantees that every used variables is valid
pub fn parse_to_llvm(config: &Config, statements: Vec<LinkedStatement>, object_factory: &ObjectFactory) -> Result<(), CE> {
    Target::initialize_all(&InitializationConfig::default());
    let context = Context::create();

    verify_main_signature(&statements, object_factory)?;

    let target_machine = create_target_machine(config);
    let target_data = target_machine.get_target_data();

    let module = module_generator::parse_module(&context, &target_data, statements, object_factory)?;
    module.set_triple(&target_machine.get_triple());

    if let Err(err) = module.verify() {
        return Err(CE::LLVMVerifyModuleError { llvm_error: err.to_string() });
    }

    create_executable(config, &target_machine, &module)?;
    Ok(())
}

fn verify_main_signature(statements: &[LinkedStatement], object_factory: &ObjectFactory) -> Result<(), CE> {
    for statement in statements {
        if let LinkedStatement::Function { object, returns, args, body: _body } = statement {
            if object_factory.get_name(*object) == "main" {
                if returns != &ObjType::Integer(IntObjType::I32) {
                    return Err(CE::IncorrectMainSignature)
                }
                if args != &vec![] {
                    return Err(CE::IncorrectMainSignature)
                }
                return Ok(())
            }
        }
    }
    Err(CE::NoMainFunction)
}

fn create_executable(config: &Config, tm: &TargetMachine, module: &Module) -> Result<(), CE> {
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

    // create object file
    if let Err(err) = tm.write_to_file(module, FileType::Object, Path::new(object_name.as_str())) {
        panic!("functions and whole module was verified, but got llvm error: {err}")
    }

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

fn create_target_machine(_config: &Config) -> TargetMachine {
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
