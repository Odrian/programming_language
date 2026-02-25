use crate::parser::parse2_syntactic::statement::{ComptimeStatement, DeclarationStatement, ExternStatement, Statement};
use crate::parser::parse3_linking::error::{collect_errors, LinkingError};
use crate::parser::parse3_linking::object::ObjType;
use crate::parser::parse3_linking::TypeContext;

pub fn parse_available_names(context: &mut TypeContext, statements: Vec<Statement>) -> Result<(), ()> {
    let errors = statements.into_iter().map(|stat|
        add_name(context, stat)
    ).collect::<Vec<_>>();
    collect_errors(&context.factory, errors)
}

fn add_name(context: &mut TypeContext, stat: Statement) -> Result<(), LinkingError> {
    match &stat {
        Statement::DeclarationStatement { name, statement } => {
            let object = context.factory.create_object(name.clone(), ObjType::Unknown);
            let object_option = context.available_names.insert(name.clone(), object);
            if object_option.is_some() {
                return Err(LinkingError::Overloading { name: name.clone() });
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
            let object_option = context.available_names.insert(name.clone(), object);
            if object_option.is_some() {
                return Err(LinkingError::Overloading { name: name.clone() });
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
    Ok(())
}
