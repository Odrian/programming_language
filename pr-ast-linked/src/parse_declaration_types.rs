use crate::error::LinkingError;
use crate::linked_statement::GlobalLinkedStatement;
use crate::object::{parse_primitive_type, ObjType, Object, ObjectFactory};
use crate::ModuleLiningContext;
use pr_ast::statement::{DeclarationStatement, ExternStatement, RStatement, RTypee, Statement, Typee};
use pr_common::error::{Diagnostic, ErrorQueue};
use pr_common::ranged::RString;
use std::cell::RefCell;
use std::collections::HashMap;

pub fn parse_declaration_types(errors: &mut ErrorQueue, context: &mut ModuleLiningContext) {
    let factory = &mut context.factory;

    for file in &context.files {
        let mut linking_context = GlobalDeclarationContext {
            factory,
            errors: RefCell::new(errors),
            type_statements: &context.type_statements,
            object_context_window: &file.available_names,
        };

        file.extern_statements.iter().for_each(|(&object, statement)| {
            let _ = linking_context.link_global_declaration(object, statement);
        });
        file.variable_statement.iter().for_each(|(&object, statement)| {
            let _ = linking_context.prelink_global_variable(object, statement);
        });
        file.function_statement.iter().for_each(|(&object, statement)| {
            let _ = linking_context.prelink_global_function(object, statement);
        });
    }
}

struct GlobalDeclarationContext<'a> {
    factory: &'a mut ObjectFactory,
    errors: RefCell<&'a mut ErrorQueue>,
    type_statements: &'a HashMap<Object, GlobalLinkedStatement>,
    object_context_window: &'a HashMap<String, Object>,
}

impl GlobalDeclarationContext<'_> {
    fn add_diag(&self, diag: Diagnostic) {
        self.errors.borrow_mut().add_diag(diag)
    }
    fn link_global_declaration(&mut self, object: Object, statement: &RStatement) -> Result<(), ()> {
        let Statement::ExternStatement { statement } = &statement.value else { unreachable!() };
        match statement {
            ExternStatement::Variable { name: _, typee } => {
                let obj_type = self.parse_type(typee)?;
                *self.factory.get_type_mut(object) = obj_type;
                Ok(())
            }
            ExternStatement::Function { name: _, args, is_vararg, returns } => {
                let arguments_type = args.iter().map(|typee| {
                    let object_type = self.parse_type(typee)?;
                    if object_type.is_void() {
                        self.add_diag(
                            LinkingError::unexpected_void_use(typee.range));
                        return Err(())
                    }
                    Ok(object_type)
                }).collect::<Result<Vec<_>, _>>()?;
                let return_type = {
                    match returns.clone() {
                        Some(typee) => self.parse_type(&typee)?,
                        None => ObjType::Void,
                    }
                };
                let func_type = ObjType::Function {
                    arguments: arguments_type,
                    is_vararg: *is_vararg,
                    returns: Box::new(return_type.clone()),
                };

                *self.factory.get_type_mut(object) = func_type;
                Ok(())
            }
        }
    }
    fn prelink_global_variable(&mut self, object: Object, statement: &RStatement) -> Result<(), ()> {
        let Statement::DeclarationStatement { name, statement } = &statement.value else { unreachable!() };
        let DeclarationStatement::VariableDeclaration { typee, value: _ } = statement else { unreachable!() };

        let Some(typee) = typee else {
            self.add_diag(
                LinkingError::global_variable_without_type(name.clone()));
            return Err(());
        };
        let obj_type = self.parse_type(typee)?;

        *self.factory.get_type_mut(object) = obj_type;
        Ok(())
    }
    fn prelink_global_function(&mut self, object: Object, statement: &RStatement) -> Result<(), ()> {
        let Statement::DeclarationStatement { name: _, statement } = &statement.value else { unreachable!() };
        let DeclarationStatement::Function { args, returns, body: _ } = statement else { unreachable!() };

        let mut arguments_type = Vec::with_capacity(args.len());
        for (_, typee) in args {
            let object_type = self.parse_type(typee)?;
            if object_type.is_void() {
                self.add_diag(
                    LinkingError::unexpected_void_use(typee.range));
                return Err(())
            }
            arguments_type.push(object_type);
        }

        let return_type = {
            match returns.clone() {
                Some(typee) => self.parse_type(&typee)?,
                None => ObjType::Void,
            }
        };

        let func_type = ObjType::Function {
            arguments: arguments_type,
            returns: Box::new(return_type.clone()),
            is_vararg: false
        };
        *self.factory.get_type_mut(object) = func_type;

        Ok(())
    }

    // simalar to parser_linked_statement::parse_type
    fn parse_type(&self, typee: &RTypee) -> Result<ObjType, ()> {
        match &typee.value {
            Typee::String(string) => {
                let primitive_option = parse_primitive_type(string);
                if let Some(object_type) = primitive_option {
                    return Ok(object_type)
                }

                if let Some(&type_object) = self.object_context_window.get(string) {
                    let Some(statement) = self.type_statements.get(&type_object) else {
                        self.add_diag(LinkingError::name_not_found(RString::new(string.clone(), typee.range)));
                        return Err(())
                    };
                    match statement {
                        GlobalLinkedStatement::Struct { .. } => return Ok(ObjType::Struct(type_object)),
                        GlobalLinkedStatement::Function { .. }
                        | GlobalLinkedStatement::VariableDeclaration { .. }
                        | GlobalLinkedStatement::ExternStatement { .. } => unreachable!(),
                    }
                };

                self.add_diag(LinkingError::name_not_found(RString::new(string.clone(), typee.range)));
                Err(())
            }
            Typee::Pointer(obj_type) => {
                let obj_type = self.parse_type(obj_type)?;
                Ok(ObjType::new_pointer(obj_type))
            }
            Typee::Reference(obj_type) => {
                let obj_type = self.parse_type(obj_type)?;
                Ok(ObjType::new_reference(obj_type))
            }
        }
    }
}
