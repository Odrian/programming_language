use crate::dependency_resolver::DependencyResolver;
use crate::error::LinkingError;
use crate::linked_statement::GlobalLinkedStatement;
use crate::object::{parse_primitive_type, ObjType, Object};
use crate::ModuleLiningContext;
use pr_ast::statement::{DeclarationStatement, RStatement, RTypee, Statement, Typee};
use pr_common::error::{Diagnostic, ErrorQueue};
use pr_common::ranged::RString;
use std::collections::HashMap;

pub fn parse_types(errors: &mut ErrorQueue, context: &mut ModuleLiningContext) {
    let mut resolver = TypeResolver::new(errors, context);

    let _ = resolver.parse_all();
}

struct TypeResolver<'c> {
    errors: &'c mut ErrorQueue,
    context: &'c mut ModuleLiningContext,
    statements: HashMap<Object, (RStatement, usize)>,
    dependency_resolver: DependencyResolver<Object>
}

impl<'c> TypeResolver<'c> {
    fn new(errors: &'c mut ErrorQueue, context: &'c mut ModuleLiningContext) -> TypeResolver<'c> {
        let mut dependency_resolver: DependencyResolver<Object> = Default::default();

        let mut statements = HashMap::new();

        for file in context.files.iter_mut() {
            let file_statements = std::mem::take(&mut file.type_statements);

            dependency_resolver.add(file_statements.keys().copied());
            statements.extend(file_statements.into_iter());
        }

        Self {
            errors,
            context,
            statements,
            dependency_resolver,
        }
    }
}

impl TypeResolver<'_> {
    fn parse_all(&mut self) -> Result<(), ()> {
        while !self.is_end() {
            self.try_parse()?;
        }
        Ok(())
    }
    fn is_end(&self) -> bool {
        self.dependency_resolver.is_empty()
    }
    fn try_parse(&mut self) -> Result<(), ()> {
        // .next return error if there is dependency cycle
        let object_option = match self.dependency_resolver.next() {
            Ok(v) => v,
            Err(err_object) => {
                let (statement, _file_id) = self.statements.get(&err_object).unwrap();
                let Statement::DeclarationStatement { name, statement: _ } = &statement.value else { unreachable!() };
                self.errors.add_diag(LinkingError::dependency_cycle(name.clone()));
                return Err(())
            }
        };
        let Some(object) = object_option else { return Ok(()) };

        let (statement, file_id) = self.statements.get(&object).unwrap();
        let file_id = *file_id;

        let Statement::DeclarationStatement { name: struct_name, statement: DeclarationStatement::Struct { fields }} = &statement.value else { unreachable!() };

        let mut dependencies = Vec::new();
        let mut linked_fields = Vec::with_capacity(fields.len());
        let mut field_names = HashMap::with_capacity(fields.len());
        for (index, (name, typee)) in fields.iter().enumerate() {
            let obj_type_result = self.parse_type(typee, file_id, &mut dependencies, false);
            let obj_type = match obj_type_result {
                Ok(k) => k,
                Err(diag) => {
                    self.errors.add_diag(diag);
                    return Err(())
                }
            };
            linked_fields.push(obj_type);
            let previous_name = field_names.insert(name.value.clone(), index as u32);
            if previous_name.is_some() {
                self.errors.add_diag(
                    LinkingError::struct_field_name_collision(struct_name.clone(), name.value.clone()));
                return Err(())
            }
        }

        if !dependencies.is_empty() {
            self.dependency_resolver.add_dependency(object, dependencies);
            return Ok(())
        }

        self.dependency_resolver.key_done(object);

        let linked_statement = GlobalLinkedStatement::new_struct(linked_fields, field_names);
        self.context.type_statements_order.push(object);
        self.context.type_statements.insert(object, linked_statement);
        *self.context.factory.get_type_mut(object) = ObjType::Struct(object);

        Ok(())
    }
    /// return [ObjType::Unknown] if it needs dependency and update [dependencies]
    fn parse_type(&self, typee: &RTypee, file_id: usize, dependencies: &mut Vec<Object>, is_ref: bool) -> Result<ObjType, Diagnostic> {
        match &typee.value {
            Typee::String(string) => {
                if let Some(object_type) = parse_primitive_type(string) {
                    return Ok(object_type)
                }
                let file = &self.context.files[file_id];

                let Some(&object) = file.available_names.get(string) else {
                    return Err(LinkingError::name_not_found(RString::new(string.clone(), typee.range)))
                };

                if !self.statements.contains_key(&object) {
                    return Err(LinkingError::name_not_found(RString::new(string.clone(), typee.range)))
                }

                if is_ref || self.context.type_statements.contains_key(&object) {
                    Ok(ObjType::Struct(object))
                } else {
                    dependencies.push(object);
                    Ok(ObjType::Unknown)
                }
            }
            Typee::Pointer(obj_type) => {
                let obj_type = self.parse_type(obj_type, file_id, dependencies, true)?;
                Ok(ObjType::new_pointer(obj_type))
            }
            Typee::Reference(obj_type) => {
                let obj_type = self.parse_type(obj_type, file_id, dependencies, true)?;
                Ok(ObjType::new_reference(obj_type))
            }
        }
    }
}
