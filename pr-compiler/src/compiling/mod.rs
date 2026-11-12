mod context_window;
mod module_generator;

use crate::CompileConfig;
use crate::error::LLVMError;
use inkwell::targets::TargetTriple;
use inkwell::{
    OptimizationLevel,
    context::Context,
    module::Module,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
};
use pr_ast_linked::LinkedModule;
use pr_ast_linked::linked_statement::GlobalLinkedStatement;
use pr_ast_linked::object::{IntObjType, ObjType};
use pr_common::{TargetArch, TargetOs};
use std::path::Path;
use std::process::Command;

const TARGET_INIT_CONFIG: InitializationConfig = InitializationConfig {
    asm_parser: true,
    asm_printer: true,
    base: true,
    disassembler: false,
    info: true,
    machine_code: true,
};

/// previous steps guarantees that every used variables is valid
pub fn parse_to_llvm(config: &CompileConfig, linked_module: LinkedModule) -> Result<(), LLVMError> {
    match config.target.target_arch {
        TargetArch::Aarch64 => Target::initialize_aarch64(&TARGET_INIT_CONFIG),
        TargetArch::X86_64 => Target::initialize_x86(&TARGET_INIT_CONFIG),
    }
    let context = Context::create();

    verify_have_main_signature(&linked_module)?;

    let target_machine = create_target_machine(config);
    let target_data = target_machine.get_target_data();
    assert_eq!(
        target_data.get_pointer_byte_size(None) * 8,
        config.target.pointer_width as u32
    );

    let module = module_generator::parse_file(config, &context, &target_data, linked_module)?;
    module.set_triple(&target_machine.get_triple());
    module.set_data_layout(&target_data.get_data_layout());

    if let Err(err) = module.verify() {
        return Err(LLVMError::llvm_verify_module_error(err.to_string()));
    }

    create_executable(config, &target_machine, &module)?;
    Ok(())
}

fn verify_have_main_signature(linked_module: &LinkedModule) -> Result<(), LLVMError> {
    let mut found = false;
    for (linked_file, _) in &linked_module.files {
        for (object, statement) in &linked_file.function_statement {
            if let GlobalLinkedStatement::Function {
                returns,
                args,
                body: _body,
            } = statement
            {
                let name = linked_module.factory.get_name(*object);
                if name.value == "main" {
                    if returns != &ObjType::Integer(IntObjType::I32) {
                        return Err(LLVMError::incorrect_main_signature(name.range));
                    }
                    if args != &vec![] {
                        return Err(LLVMError::incorrect_main_signature(name.range));
                    }
                    if found {
                        return Err(LLVMError::second_main_function(name.range));
                    }
                    found = true;
                }
            }
        }
    }
    if found {
        return Ok(());
    }
    Err(LLVMError::no_main_function())
}

fn create_executable(config: &CompileConfig, tm: &TargetMachine, module: &Module) -> Result<(), LLVMError> {
    let args = &config.args;
    let assembly_name = format!("{}.ll", args.exe_name);
    let object_name = format!("{}.o", args.exe_name);
    let executable_name = args.exe_name.clone();

    // create assembly file
    #[expect(clippy::collapsible_if)]
    if args.gen_llvm {
        if let Err(err) = module.print_to_file(assembly_name) {
            return Err(LLVMError::llvm_failed_to_create_assembly(err.to_string()));
        }
    }

    if !args.gen_object && args.no_exe {
        return Ok(());
    }

    // create object file
    if let Err(err) = tm.write_to_file(module, FileType::Object, Path::new(object_name.as_str())) {
        panic!("functions and whole module was verified, but got llvm error: {err}")
    }

    let status_option = if !args.no_exe {
        let mut command = linker_command(config.target.target_os, &object_name, &executable_name);
        let linker = command.get_program().to_string_lossy().into_owned();
        Some((linker, command.status()))
    } else {
        None
    };

    #[expect(clippy::collapsible_if)]
    if !args.gen_object {
        if let Err(err) = std::fs::remove_file(object_name.clone()) {
            return Err(LLVMError::cant_delete_object_file(object_name, err.to_string()));
        }
    }

    if let Some((linker, status)) = status_option {
        // parse linking result
        match status {
            Ok(exit_status) => {
                if !exit_status.success() {
                    let exit_code = exit_status
                        .code()
                        .map(|c| c.to_string())
                        .unwrap_or_else(|| "unknown".to_owned());
                    let description = format!("exit with code {exit_code}");
                    return Err(LLVMError::linker_error(&linker, description));
                }
            }
            Err(err) => return Err(LLVMError::linker_error(&linker, err.to_string())),
        }
    }
    Ok(())
}

fn linker_command(target_os: TargetOs, object_name: &str, executable_name: &str) -> Command {
    match target_os {
        TargetOs::Linux | TargetOs::Macos => {
            let mut command = Command::new("cc");
            command.args([object_name, "-o", executable_name]);
            command
        }
        TargetOs::Windows => {
            let mut command = Command::new("link.exe");
            command
                .args(["/NOLOGO", "/SUBSYSTEM:CONSOLE"])
                .arg(format!("/OUT:{executable_name}"))
                .arg(object_name)
                // IDK what it is, but it's probably necessary
                .args([
                    "/DEFAULTLIB:libcmt",
                    "/DEFAULTLIB:libvcruntime",
                    "/DEFAULTLIB:libucrt",
                    "/DEFAULTLIB:legacy_stdio_definitions",
                    "/DEFAULTLIB:kernel32",
                ]);
            command
        }
    }
}

fn create_target_machine(config: &CompileConfig) -> TargetMachine {
    let triple = TargetTriple::create(config.target.llvm_target);
    let target = Target::from_triple(&triple).unwrap_or_else(|err| {
        panic!(
            "Target should be created from compile time triple {}: {err}",
            config.target.llvm_target
        )
    });
    let cpu = "generic";
    let features = "";

    target
        .create_target_machine(
            &triple,
            cpu,
            features,
            OptimizationLevel::None,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .expect("Target machine should be created from compile time config ")
}
