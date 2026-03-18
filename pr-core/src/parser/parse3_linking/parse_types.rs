use crate::parser::parse2_syntactic::statement::{DeclarationStatement, Statement, RStatement, Typee, RTypee};
use crate::parser::parse3_linking::dependency_resolver::DependencyResolver;
use crate::parser::parse3_linking::linked_statement::GlobalLinkedStatement;
use crate::parser::parse3_linking::object::{parse_primitive_type, ObjType, Object};
use crate::parser::parse3_linking::TypeContext;
use std::collections::HashMap;
use crate::error::{Diagnostic, ErrorQueue};
use crate::parser::parse3_linking::error::LinkingError;
use crate::RString;

pub fn parse_types(errors: &mut ErrorQueue, context: &mut TypeContext) {
    let statements = std::mem::take(&mut context.type_statements);
    let mut resolver = TypeResolver::new(errors, context, statements);

    let _ = resolver.parse_all();
}

struct TypeResolver<'c> {
    errors: &'c mut ErrorQueue,
    context: &'c mut TypeContext,
    statements: HashMap<Object, RStatement>,
    dependency_resolver: DependencyResolver<Object>
}

impl<'c> TypeResolver<'c> {
    fn new(errors: &'c mut ErrorQueue, context: &'c mut TypeContext, statements: HashMap<Object, RStatement>) -> TypeResolver<'c> {
        let mut dependency_resolver: DependencyResolver<Object> = Default::default();
        dependency_resolver.add(statements.keys().copied());
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
                let statement = self.statements.get(&err_object).unwrap();
                let Statement::DeclarationStatement { name, statement: _ } = &statement.value else { unreachable!() };
                self.errors.add_diag(LinkingError::dependency_cycle(name.clone()));
                return Err(())
            }
        };
        let Some(object) = object_option else { return Ok(()) };

        let statement = self.statements.get(&object).unwrap();

        let Statement::DeclarationStatement { name: struct_name, statement: DeclarationStatement::Struct { fields }} = &statement.value else { unreachable!() };

        let mut dependencies = Vec::new();
        let mut linked_fields = Vec::with_capacity(fields.len());
        let mut field_names = HashMap::with_capacity(fields.len());
        for (index, (name, typee)) in fields.iter().enumerate() {
            let obj_type_result = self.parse_type(typee, &mut dependencies, false);
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
        self.context.result.type_statements_order.push(object);
        self.context.result.type_statements.insert(object, linked_statement);

        Ok(())
    }
    /// return [ObjType::Unknown] if it needs dependency and update [dependencies]
    fn parse_type(&self, typee: &RTypee, dependencies: &mut Vec<Object>, is_ref: bool) -> Result<ObjType, Diagnostic> {
        match &typee.value {
            Typee::String(string) => {
                if let Some(object_type) = parse_primitive_type(string) {
                    return Ok(object_type)
                }

                let Some(&object) = self.context.available_names.get(string) else {
                    return Err(LinkingError::name_not_found(RString::new(string.clone(), typee.range)))
                };

                if !self.statements.contains_key(&object) {
                    return Err(LinkingError::name_not_found(RString::new(string.clone(), typee.range)))
                }

                if !is_ref && !self.context.result.type_statements.contains_key(&object) {
                    dependencies.push(object);
                    return Ok(ObjType::Unknown)
                }

                Ok(ObjType::Struct(object))
            }
            Typee::Pointer(obj_type) => {
                let obj_type = self.parse_type(obj_type, dependencies, true)?;
                Ok(ObjType::new_pointer(obj_type))
            }
            Typee::Reference(obj_type) => {
                let obj_type = self.parse_type(obj_type, dependencies, true)?;
                Ok(ObjType::new_reference(obj_type))
            }
        }
    }
}
