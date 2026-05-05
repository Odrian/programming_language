use inkwell::builder::BuilderError;
use lsp_types::Range;
use pr_common::error::{Diagnostic, DiagnosticString};
use std::path::PathBuf;


pub enum LLVMError {
    BuilderError(BuilderError),
    Error(String),
    RangedError(String, Range),
}

impl LLVMError {
    pub fn cant_delete_object_file(filepath: String, io_error: String) -> Self {
        Self::Error(format!(
            "while deleting object file {filepath}. Error: {io_error}"
        ))
    }
    pub fn no_main_function() -> Self {
        Self::Error(
            "Error: No main function".to_string()
        )
    }
    pub fn incorrect_main_signature(range: Range) -> Self {
        Self::RangedError(
            "Error: incorrect main signature, only () -> Diagnostic i32 allowed".to_string(),
            range,
        )
    }
    pub fn global_with_value(name: String) -> LLVMError {
        Self::Error(format!(
            "Error: global '{name}' has value, use ---"
        ))
    }

    pub fn llvm_verify_function_error(name: String) -> LLVMError {
        Self::Error(format!(
            "Error: function {name} probably doesn't return anything, read llvm error below"
        ))
    }
    pub fn llvm_verify_module_error(llvm_error: String) -> LLVMError {
        Self::Error(format!(
            "Error: module verify error: {llvm_error}"
        ))
    }
    pub fn llvm_failed_to_create_assembly(llvm_error: String) -> LLVMError {
        Self::Error(format!(
            "Error: failed to create assembly file: {llvm_error}"
        ))
    }
    pub fn linker_error(description: String) -> LLVMError {
        Self::Error(format!(
            "Error: failed to run 'gcc': {description}"
        ))
    }

    pub fn source_file_reading_error(filepath: PathBuf, io_error: String) -> LLVMError {
        Self::Error(format!("while reading file {}: {io_error}", filepath.display()))
    }
    pub fn file_writing_error(filepath: String, what: String, io_error: String) -> LLVMError {
        Self::Error(format!("while writing {what} to file {filepath}: {io_error}"))
    }
    pub fn parse_int_literal_error(literal: &str, error: &str) -> Self {
        Self::Error(format!("incorrect int literal \"{literal}\": {error}"))
    }
}


impl LLVMError {
    pub fn to_diagnostic(self) -> Diagnostic {
        match self {
            Self::BuilderError(err) => DiagnosticString::new(format!("{err:?}")).to_diag0(),
            Self::Error(text) => DiagnosticString::new(text).to_diag0(),
            Self::RangedError(text, range) => DiagnosticString::new(text).to_diag(range),
        }
    }
}

impl From<BuilderError> for LLVMError {
    fn from(value: BuilderError) -> Self {
        Self::BuilderError(value)
    }
}
