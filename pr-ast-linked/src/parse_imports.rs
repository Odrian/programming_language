use std::collections::HashMap;
use pr_ast::statement::{ComptimeStatement, Statement};
use pr_common::error::ErrorQueue;
use pr_common::ranged::RString;
use crate::error::LinkingError;
use crate::GlobalLiningContext;
use crate::object::Object;

pub fn parse_imports(errors: &mut ErrorQueue, context: &mut GlobalLiningContext) {
    let mut path_map = HashMap::new();
    for (index, path) in context.module_paths.iter().cloned().enumerate() {
        path_map.insert(path, index);
    }

    let imports = context.modules.iter_mut().map(|module|
        std::mem::take(&mut module.import_statements)
    ).collect::<Vec<_>>();

    let mut new_names: Vec<Vec<(RString, Object)>> = vec![Default::default(); imports.len()];

    for (module_id, import_statements) in imports.into_iter().enumerate() {
        let module_path = &context.module_paths[module_id];
        for import_statement in import_statements {
            let Statement::ComptimeStatement(statement) = import_statement.value else { unreachable!() };
            let ComptimeStatement::Import { from, what } = statement;

            let path = find_module(module_path, from);
            let Some(&from_module_id) = path_map.get(&path) else {
                errors.add_diag(LinkingError::import_no_module(path, module_path, import_statement.range));
                continue
            };
            let from_module = &context.modules[from_module_id];

            for (name, as_name) in what {
                let Some(&object) = from_module.available_names.get(&name.value) else {
                    errors.add_diag(LinkingError::import_no_name(name.value, &path, import_statement.range));
                    continue
                };

                match as_name {
                    Some(name) => {
                        new_names[module_id].push((name, object));
                    }
                    None => {
                        new_names[module_id].push((name, object));
                    }
                }
            }
        }
    }

    for (module_id, new_imports) in new_names.into_iter().enumerate() {
        let available_names = &mut context.modules[module_id].available_names;
        for (key, value) in new_imports {
            if available_names.insert(key.value, value).is_some() {
                errors.add_diag(LinkingError::import_global_name_overlap(key.range, &context.module_paths[module_id]))
            }
        }
    }
}

fn find_module(current_module: &String, from: Vec<RString>) -> String {
    let module = &from[0].value;

    if module != "self" {
        // TODO: try to find module
    }

    parse_path(current_module.clone(), from.into_iter())
}

fn parse_path(module_path: String, from: impl Iterator<Item=RString>) -> String {
    let mut path = module_path;

    for delta in from {
        let delta = delta.value;

        match delta.as_str() {
            "super" => {
                if let Some(index) = path.rfind("\\") {
                    path.truncate(index)
                } else {
                    panic!("can't use super out of working directory");
                    // path.clear();
                    // path.push_str("..");
                }
            }
            "self" => {}
            _ => {
                path.push('\\');
                path.push_str(&delta);
            }
        }
    }

    path
}
