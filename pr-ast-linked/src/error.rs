use pr_common::error::{Diagnostic, DiagnosticString};
use pr_common::operations::{ROneSidedOperation, RTwoSidedOperation};
use pr_common::ranged::RString;
use lsp_types::Range;

pub struct LinkingError;

impl LinkingError {
    pub fn dependency_cycle(name: RString) -> Diagnostic {
        DiagnosticString::new(
            format!("dependency cycle with {name}")
        ).to_diag(name.range)
    }
    pub fn overloading(name: RString) -> Diagnostic {
        DiagnosticString::new(
            format!("overloaded {name}")
        ).to_diag(name.range)
    }
    pub fn name_not_found(name: RString) -> Diagnostic {
        DiagnosticString::new(
            format!("undefined name {name}")
        ).to_diag(name.range)
    }
    pub fn call_not_function(name: RString) -> Diagnostic {
        DiagnosticString::new(
            format!("can't call {name}, it's not a function")
        ).to_diag(name.range)
    }

    pub fn dot_not_on_struct(got_obj_type: String, range: Range) -> Diagnostic {
        DiagnosticString::new(
            format!("dot operator can't be used on {got_obj_type}")
        ).to_diag(range)
    }
    pub fn struct_field_name_collision(struct_name: RString, field_name: String) -> Diagnostic {
        DiagnosticString::new(
            format!("construction of struct '{struct_name}' has two fields with name {field_name}")
        ).to_diag(struct_name.range)
    }
    pub fn struct_field_name_collision_in_construction(struct_name: RString, field_name: String) -> Diagnostic {
        DiagnosticString::new(
            format!("struct '{struct_name}' has two fields with name {field_name}")
        ).to_diag(struct_name.range)
    }
    pub fn struct_field_name_not_found(struct_name: String, field_name: RString) -> Diagnostic {
        DiagnosticString::new(
            format!("struct '{struct_name}' hasn't field '{field_name}'")
        ).to_diag(field_name.range)
    }
    pub fn struct_field_missing_in_construction(struct_name: RString, field_name: String) -> Diagnostic {
        DiagnosticString::new(
            format!("no field '{field_name}' in struct '{struct_name}' construction")
        ).to_diag(struct_name.range)
    }
    pub fn incorrect_type(got_obj_type: String, expected_obj_type: String, range: Range) -> Diagnostic {
        DiagnosticString::new(
            format!("incorrect type, got {got_obj_type}, expected {expected_obj_type}")
        ).to_diag(range)
    }
    pub fn cant_determine_type(range: Range) -> Diagnostic {
        DiagnosticString::from_text(
            "can't determine type"
        ).to_diag(range)
    }
    pub fn incorrect_one_oper(obj_type: String, op: ROneSidedOperation) -> Diagnostic {
        DiagnosticString::new(
            format!("can't use '{op}' to '{obj_type}'")
        ).to_diag(op.range)
    }
    pub fn incorrect_two_oper(obj_type1: String, obj_type2: String, op: RTwoSidedOperation) -> Diagnostic {
        DiagnosticString::new(
            format!("can't use '{op}' between '{obj_type1}' and '{obj_type2}'")
        ).to_diag(op.range)
    }
    pub fn incorrect_as(obj_type_from: String, obj_type_to: String, range: Range) -> Diagnostic {
        DiagnosticString::new(
            format!("can't cast {obj_type_from} to {obj_type_to}")
        ).to_diag(range)
    }
    pub fn global_variable_without_type(name: RString) -> Diagnostic {
        DiagnosticString::new(
            format!("global variable '{name}' is declared without type annotation")
        ).to_diag(name.range)
    }
    pub fn unexpected_void_use(range: Range) -> Diagnostic {
        DiagnosticString::from_text(
            "'void' can't be used as actual type"
        ).to_diag(range)
    }

    pub fn function_must_return(function_name: RString) -> Diagnostic {
        DiagnosticString::new(
            format!("function {function_name} may not return")
        ).to_diag(function_name.range)
    }

    pub fn literal_unexpected_suffix(suffix: String, range: Range) -> Diagnostic {
        DiagnosticString::new(
            format!("unexpected suffix {suffix} in literal")
        ).to_diag(range)
    }
    pub fn incorrect_argument_count(function_name: RString, is_vararg: bool, argument_need: usize, argument_got: usize) -> Diagnostic {
        let at_least = if is_vararg { " at least" } else { "" };
        DiagnosticString::new(
            format!("incorrect argument count for function {function_name}, need{at_least} {argument_need}, got {argument_got}")
        ).to_diag(function_name.range)
    }
    pub fn function_as_value(name: RString) -> Diagnostic {
        DiagnosticString::new(
            format!("can't use function {name} as variable value")
        ).to_diag(name.range)
    }
    pub fn incorrect_lvalue(range: Range) -> Diagnostic {
        DiagnosticString::from_text(
            "incorrect value before ="
        ).to_diag(range)
    }
}
