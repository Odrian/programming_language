use crate::error::CResult;
use crate::parser::parse2_syntactic::statement::{DeclarationStatement, Statement, Typee};
use crate::parser::parse3_linking::dependency_resolver::DependencyResolver;
use crate::parser::parse3_linking::linked_statement::GlobalLinkedStatement;
use crate::parser::parse3_linking::object::{ObjType, Object};
use crate::parser::parse3_linking::TypeContext;
use std::collections::HashMap;
use crate::parser::parse3_linking::error::LinkingError;
use crate::parser::parse3_linking::parser_linked_statement::parse_primitive_type;

pub fn parse_types(context: &mut TypeContext) -> CResult<()> {
    let statements = std::mem::take(&mut context.type_statements);
    let mut resolver = TypeResolver::new(context, statements);

    while !resolver.is_end() {
        resolver.try_parse()?;
    }

    Ok(())
}

struct TypeResolver<'c> {
    context: &'c mut TypeContext,
    statements: HashMap<Object, Statement>,
    dependency_resolver: DependencyResolver<Object>
}

impl<'c> TypeResolver<'c> {
    fn new(context: &'c mut TypeContext, statements: HashMap<Object, Statement>) -> TypeResolver<'c> {
        let mut dependency_resolver: DependencyResolver<Object> = Default::default();
        dependency_resolver.add(statements.keys().copied());
        Self {
            context,
            statements,
            dependency_resolver,
        }
    }
}

impl TypeResolver<'_> {
    fn is_end(&self) -> bool {
        self.dependency_resolver.is_empty()
    }
    fn try_parse(&mut self) -> CResult<()> {
        // .next return error if there is dependency cycle
        let Some(object) = self.dependency_resolver.next()? else { return Ok(()) };

        let statement = self.statements.get(&object).unwrap();

        let Statement::DeclarationStatement { name: struct_name, statement: DeclarationStatement::Struct { fields }} = statement else { unreachable!() };

        let mut dependencies = Vec::new();
        let mut linked_fields = Vec::with_capacity(fields.len());
        let mut field_names = HashMap::with_capacity(fields.len());
        for (index, (name, typee)) in fields.iter().enumerate() {
            let obj_type = self.parse_type(typee, &mut dependencies, false)?;
            linked_fields.push(obj_type);
            let previous_name = field_names.insert(name.clone(), index as u32);
            if previous_name.is_some() {
                LinkingError::StructFieldNameCollision { struct_name: struct_name.clone(), field_name: name.clone(), in_construction: false }.print();
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
    fn parse_type(&self, typee: &Typee, dependencies: &mut Vec<Object>, is_ref: bool) -> CResult<ObjType> {
        match typee {
            Typee::String(string) => {
                if let Some(object_type) = parse_primitive_type(string) {
                    return Ok(object_type)
                }

                let Some(&object) = self.context.available_names.get(string) else {
                    LinkingError::NameNotFound { name: string.clone(), context: format!("{:?}", self.context.factory) }.print();
                    return Err(())
                };

                if !self.statements.contains_key(&object) {
                    LinkingError::NameNotFound { name: string.clone(), context: format!("{:?}", self.context.factory) }.print();
                    return Err(())
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
