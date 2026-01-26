pub mod parser;

pub mod file_span;
pub mod module_tree;
pub mod error;
pub mod io_error;

use std::path::PathBuf;
use clap::Parser;
use error::CResult;
use crate::parser::parse_to_exe;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct Args {
    /// Name of generated exe
    #[arg(value_name = "EXE")]
    pub exe_name: String,

    /// Don't generate exe
    #[arg(long, default_value_t = false)]
    pub no_exe: bool,

    /// Generate .txt with tokens
    #[arg(long, default_value_t = false)]
    pub gen_tokens: bool,

    /// Generate AST (abstract syntactic tree)
    #[arg(long, default_value_t = false)]
    pub gen_ast: bool,

    /// Generate linked AST (abstract syntactic tree)
    #[arg(long, default_value_t = false)]
    pub gen_last: bool,

    /// Generate .ll files
    #[arg(long, default_value_t = false)]
    pub gen_llvm: bool,

    /// Generate .o files
    #[arg(long, default_value_t = false)]
    pub gen_object: bool,
}

pub fn compile_src(args: &Args, path: PathBuf) -> CResult<()> {
    let file_path = path.join("main.txt");

    parse_to_exe(args, file_path)?;

    Ok(())
}

/*
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

 */