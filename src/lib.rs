pub mod parser;

pub mod file_span;
pub mod module_tree;
pub mod error;
pub mod io_error;

use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use clap::Parser;
use error::CResult;
use crate::module_tree::{ModuleId, ModuleMetadata, ModuleTree, RootMetadata};
use crate::parser::{parse_statements_single_file, parse_to_statements};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct Args {
    pub output: String,

    #[arg(long, default_value_t = false)]
    pub dont_create_executable: bool,

    #[arg(long, default_value_t = false)]
    pub write_tokens_to_file: bool,
    #[arg(long, default_value_t = false)]
    pub write_unlinked_syntactic_tree_to_file: bool,
    #[arg(long, default_value_t = false)]
    pub write_syntactic_tree_to_file: bool,
    #[arg(long, default_value_t = false)]
    pub create_llvm_ir: bool,
    #[arg(long, default_value_t = false)]
    pub create_object: bool,
}

const SOURCE_ROOT_NAME: &str = "crete";

pub fn compile_src(args: &Args, tree: &mut ModuleTree, path: PathBuf) -> CResult<()> {
    let name = SOURCE_ROOT_NAME.to_owned();
    let metadata = RootMetadata { path };
    compile_root_module(args, tree, name, metadata)
}

pub fn compile_root_module(args: &Args, tree: &mut ModuleTree, name: String, metadata: RootMetadata) -> CResult<()> {
    let files = parse_root_module(tree, name, metadata);

    let mut statements = HashMap::new();
    for &file_id in &files {
        let file_statements = parse_to_statements(args, tree, file_id)?;
        statements.insert(file_id, file_statements);
    }

    if files.len() != 1 { unimplemented!("can't parse multiple files") }
    for file_id in files {
        parse_statements_single_file(args, &tree.get_metadata(file_id).name, statements.remove(&file_id).unwrap())?;
    }

    Ok(())
}

/// @returns Vec<ModuleId> of files
fn parse_root_module(tree: &mut ModuleTree, name: String, metadata: RootMetadata) -> Vec<ModuleId> {
    let path = metadata.path.clone();
    let module_metadata = ModuleMetadata { name, is_file: false };
    let module_id = tree.create_root_or_panic(module_metadata, metadata);
    parse_module(tree, module_id, path)
}

fn parse_module(tree: &mut ModuleTree, module_id: ModuleId, path: PathBuf) -> Vec<ModuleId> {
    let Ok(entries) = fs::read_dir(path) else { return vec![]; };
    entries.flatten().flat_map(|entry| {
        let Ok(metadata) = entry.metadata() else { return vec![] };
        if metadata.is_file() {
            let name = entry.file_name().to_str().unwrap().to_owned();
            let metadata = ModuleMetadata { name, is_file: true };
            let new_module_id = tree.create_child_or_panic(module_id, metadata);
            return vec![new_module_id];
        }
        if metadata.is_dir() {
            let name = entry.file_name().to_str().unwrap().to_owned();
            let metadata = ModuleMetadata { name, is_file: false };
            let new_module_id = tree.create_child_or_panic(module_id, metadata);
            return parse_module(tree, new_module_id, entry.path());
        }
        vec![]
    }).collect()
}
