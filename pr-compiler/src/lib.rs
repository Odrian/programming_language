use std::ffi::OsStr;
use std::fs;
use std::path::PathBuf;
use clap::Parser;
use pr_common::error::{range_to_str, ErrorQueue};
use pr_common::ranged_tree::NodeRef;
use pr_lexer::token::Token;
use pr_lexer::{TokenIter, TokenLinearTree};
use pr_ast::SyntacticResult;
use pr_ast_linked::LinkedFile;
use pr_common::Target;
use crate::error::LLVMError;

pub mod compiling;
mod error;

pub struct CompileConfig {
    pub args: Args,
    pub target: Target,
}

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


pub fn compile_src(errors: &mut ErrorQueue, config: &CompileConfig, base_path: PathBuf) -> Result<(), ()> {
    let mut files = Vec::new();
    find_files(&mut files, &base_path, PathBuf::new());

    fn find_files(files: &mut Vec<PathBuf>, base_path: &PathBuf, path: PathBuf) {
        let Ok(entries) = fs::read_dir(base_path.join(&path)) else { return };
        for entry_result in entries {
            let Ok(entry) = entry_result else { return };
            let Ok(metadata) = entry.metadata() else { return };

            if metadata.is_file() {
                if entry.path().extension() != Some(OsStr::new("pr")) { continue; }
                let name = entry.file_name();

                let mut file_path = path.clone();
                file_path.push(name);
                files.push(file_path);
            }
            if metadata.is_dir() {
                let name = entry.file_name().to_str().unwrap().to_owned();

                let mut dir_path = path.clone();
                dir_path.push(name);

                return find_files(files, base_path, dir_path);
            }
        }
    }

    let statements = files.into_iter().map(|path| {
        parse_to_statements(errors, config, base_path.join(&path))
            .map(|result| {
                let path = path.to_str().expect("valid utf name").to_string();
                (path, result)
            })
    }).collect::<Result<Vec<_>, _>>()?;

    let linked_module = pr_ast_linked::link_module(errors, statements);
    if config.args.gen_last { linked_module.files.iter().for_each(|(file, path)|
        generate_last_file(errors, &path, file)
    ) }

    if errors.has_errors() { return Err(()) }

    todo!("compile multimodule");
    // compiling::parse_to_llvm(args, linked_module)
    //     .map_err(|err| ErrorQueue::new_single_diag(err.to_diagnostic()))?;

    Ok(())
}

pub fn compile_file(errors: &mut ErrorQueue, config: &CompileConfig, file_path: PathBuf) -> Result<(), ()> {
    let filename = file_path.file_name().unwrap().to_str().unwrap().to_owned();

    let statements = parse_to_statements(errors, config, file_path)?;

    if errors.has_errors() { return Err(()) }

    let linked_file = pr_ast_linked::link_file(errors, statements);
    if config.args.gen_last { generate_last_file(errors, &filename, &linked_file) }

    if errors.has_errors() { return Err(()) }

    compiling::parse_to_llvm(config, linked_file)
        .map_err(|err| errors.add_diag(err.to_diagnostic()))?;

    Ok(())
}

fn parse_to_statements(errors: &mut ErrorQueue, config: &CompileConfig, file_path: PathBuf) -> Result<SyntacticResult, ()> {
    let filename = file_path.file_name().unwrap().to_str().unwrap().to_owned();
    let text = fs::read_to_string(&file_path)
        .map_err(|err| {
            errors.add_diag(
                LLVMError::source_file_reading_error(file_path, err.to_string())
                    .to_diagnostic());
            ()
        })?;

    let tokens = pr_lexer::tokenize(errors, &text);
    if config.args.gen_tokens { generate_tokens_file(errors, &filename, &tokens) }

    let statements = pr_ast::parse_ast(errors, &config.target, tokens);
    if config.args.gen_ast { generate_ast_file(errors, &filename, &statements) }

    Ok(statements)
}

const ARTIFACT_DIR: &str = "artifacts";

fn tokens_to_str(tokens: TokenIter<'_>, layer: u8) -> String {
    tokens.map(|(token, range)| {
        let range = range_to_str(*range);
        let str = match token {
            NodeRef::Elem(elem) => match elem {
                Token::String(str) => format!("{str} {range}"),
                Token::DoubleQuotes(str) => format!("\"{str}\" {range}"),
                Token::Quotes(str) => format!("'{str}' {range}"),
                Token::Keyword(keyword) => format!("{keyword:?} {range}"),
                Token::NumberLiteral(str) => format!("{str} {range}"),

                Token::UnaryOperation(op) => format!("{op} {range}"),
                Token::Operation(op) => format!("{op} {range}"),
                Token::EqualOperation(op) => format!("{op} {range}"),

                Token::Semicolon => format!("; {range}"),
                Token::Dot => format!(". {range}"),
                Token::DoubleDot => format!(".. {range}"),
                Token::Comma => format!(", {range}"),
                Token::Colon => format!(": {range}"),
                Token::DoubleColon => format!(":: {range}"),
                Token::Arrow => format!("-> {range}"),
            }
            NodeRef::Block(bracket, body) => {
                let body = tokens_to_str(body, layer + 1);
                let left = bracket.to_open_string();
                let right = bracket.to_close_string();
                let spaces = "    ".repeat(layer as usize);
                format!("{left} {range}\n{body}\n{spaces}{right}")
            }
        };
        "    ".repeat(layer as usize).to_owned() + &str
    }).collect::<Vec<_>>().join("\n")
}

fn generate_tokens_file(errors: &mut ErrorQueue, filename: &String, tokens: &TokenLinearTree) {
    let text = tokens_to_str(tokens.iter(), 0);

    fs::create_dir_all(ARTIFACT_DIR).unwrap();
    let filepath = format!("{ARTIFACT_DIR}/{filename}_tokens.txt");
    let write_result = fs::write(&filepath, text);
    if let Err(err) = write_result {
        errors.add_diag(
            LLVMError::file_writing_error(
                filepath,
                "tokens".to_owned(),
                err.to_string()
            ).to_diagnostic());
    }
}

fn generate_ast_file(errors: &mut ErrorQueue, filename: &String, statements: &SyntacticResult) {
    let text = statements.statements.iter().map(ToString::to_string).collect::<Vec<_>>().join("\n");

    fs::create_dir_all(ARTIFACT_DIR).unwrap();
    let filepath = format!("{ARTIFACT_DIR}/{filename}_AST.txt");
    let write_result = fs::write(&filepath, text);
    if let Err(err) = write_result {
        errors.add_diag(LLVMError::file_writing_error(
            filepath, "unlinked AST".to_owned(), err.to_string()
        ).to_diagnostic());
    }
}

fn generate_last_file(errors: &mut ErrorQueue, filename: &String, linked_file: &LinkedFile) {
    let text = [&linked_file.extern_statements, &linked_file.type_statements, &linked_file.variable_statement, &linked_file.function_statement]
        .iter().map(|hashmap| hashmap.iter()
        .map(|(&object, statement)| statement.to_string::<true>(&linked_file.factory, object) + "\n")
        .collect::<String>()
    ).collect::<String>();

    fs::create_dir_all(ARTIFACT_DIR).unwrap();
    let filepath = format!("{ARTIFACT_DIR}/{filename}_LAST.txt");
    let write_result = fs::write(&filepath, text);
    if let Err(err) = write_result {
        errors.add_diag(LLVMError::file_writing_error(
            filepath, "AST".to_owned(), err.to_string()
        ).to_diagnostic());
    }
}
