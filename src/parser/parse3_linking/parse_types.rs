use crate::error::CResult;
use crate::parser::parse2_syntactic::statement::{DeclarationStatement, Statement, Typee};
use crate::parser::parse3_linking::dependency_resolver::DependencyResolver;
use crate::parser::parse3_linking::linked_statement::GlobalLinkedStatement;
use crate::parser::parse3_linking::object::{ObjType, Object};
use crate::parser::parse3_linking::TypeContext;
use std::collections::HashMap;

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
        let Some(object) = self.dependency_resolver.next()? else { return Ok(()) };

        let statement = self.statements.get(&object).unwrap();

        let Statement::DeclarationStatement { name: _, statement: DeclarationStatement::Struct { fields }} = statement else { unreachable!() };

        let mut dependencies = Vec::new();
        let linked_fields = fields.iter().map(|(name, typee)| {
            let obj_type_option = self.parse_type(typee);
            let obj_type = obj_type_option.unwrap_or_else(|object| {
                dependencies.push(object);
                ObjType::Unknown
            });
            (name.clone(), obj_type)
        }).collect();

        if !dependencies.is_empty() {
            unimplemented!(); // add dependencies
            return Ok(())
        }

        self.dependency_resolver.key_done(object);

        let linked_statement = GlobalLinkedStatement::new_struct(linked_fields);
        self.context.result.type_statements.insert(object, linked_statement);

        Ok(())
    }
    /// return ObjType or return None and update dependencies
    fn parse_type(&self, typee: &Typee) -> Result<ObjType, Object> {
        unimplemented!()
    }
}
