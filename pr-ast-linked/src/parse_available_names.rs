use pr_common::error::ErrorQueue;
use pr_ast::statement::{ComptimeStatement, DeclarationStatement, ExternStatement, RStatement, Statement};
use crate::error::LinkingError;
use crate::object::ObjType;
use crate::TypeContext;

pub fn parse_available_names(errors: &mut ErrorQueue, context: &mut TypeContext, statements: Vec<RStatement>) {
    for statement in statements {
        add_name(errors, context, statement);
    }
}

/// has errors only on name overloading
fn add_name(errors: &mut ErrorQueue, context: &mut TypeContext, stat: RStatement) {
    match &stat.value {
        Statement::DeclarationStatement { name, statement } => {
            let object = context.factory.create_object(name.clone(), ObjType::Unknown);
            let object_option = context.available_names.insert(name.value.clone(), object);
            if object_option.is_some() {
                errors.add_diag(LinkingError::overloading(name.clone()));
                return
            }
            match statement {
                DeclarationStatement::VariableDeclaration { .. } => {
                    context.variable_statement.insert(object, stat);
                }
                DeclarationStatement::Function { .. } => {
                    context.function_statement.insert(object, stat);
                }
                DeclarationStatement::Struct { .. } => {
                    context.type_statements.insert(object, stat);
                }
            }
        }
        Statement::ExternStatement { statement } => {
            let name = match statement {
                ExternStatement::Variable { name, .. } => name,
                ExternStatement::Function { name, .. } => name,
            };
            let object = context.factory.create_object(name.clone(), ObjType::Unknown);
            let object_option = context.available_names.insert(name.value.clone(), object);
            if object_option.is_some() {
            errors.add_diag(LinkingError::overloading(name.clone()));
                return
            }
            context.extern_statements.insert(object, stat);
        }
        Statement::ComptimeStatement(comp_stat) => match comp_stat {
            ComptimeStatement::Import { .. } => {
                unimplemented!()
            }
        }
        Statement::SetVariable { .. } | Statement::Expression(_) | Statement::If { .. }
        | Statement::While { .. } | Statement::Return(_) | Statement::Brackets(..) => {
            unreachable!()
        }
    }
}
