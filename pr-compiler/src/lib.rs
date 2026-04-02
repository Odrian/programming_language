use std::fs;
use std::path::PathBuf;
use clap::Parser;
use pr_common::error::{range_to_str, ErrorQueue};
use pr_common::ranged_tree::NodeRef;
use pr_lexer::token::Token;
use pr_lexer::{TokenIter, TokenLinearTree};
use pr_ast::SyntacticResult;
use pr_ast_linked::LinkedProgram;
use crate::error::LLVMError;

pub mod compiling;
mod error;

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

pub fn compile_src(args: &Args, path: PathBuf) -> Result<(), ErrorQueue> {
    let file_path = path.join("main.pr");

    parse_to_exe(args, file_path)?;

    Ok(())
}


const ARTIFACT_DIR: &str = "artifacts";

pub fn parse_to_exe(args: &Args, file_path: PathBuf) -> Result<(), ErrorQueue> {
    let filename = file_path.file_name().unwrap().to_str().unwrap().to_owned();
    let text = fs::read_to_string(&file_path)
        .map_err(|err| {
            ErrorQueue::new_single_diag(
                LLVMError::source_file_reading_error(file_path, err.to_string())
                    .to_diagnostic())
        })?;
    let mut errors = ErrorQueue::default();

    let tokens = pr_lexer::tokenize(&mut errors, &text);
    if args.gen_tokens { generate_tokens_file(&filename, &tokens)? }

    let statements = pr_ast::parse_ast(&mut errors, tokens);
    if args.gen_ast { generate_ast_file(&filename, &statements)? }

    if errors.has_errors() { return Err(errors) }

    let linked_program = pr_ast_linked::link_ast(&mut errors, statements);
    if args.gen_last { generate_last_file(&filename, &linked_program)? }

    if errors.has_errors() { return Err(errors) }

    compiling::parse_to_llvm(args, linked_program)
        .map_err(|err| ErrorQueue::new_single_diag(err.to_diagnostic()))?;

    Ok(())
}

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

fn generate_tokens_file(filename: &String, tokens: &TokenLinearTree) -> Result<(), ErrorQueue> {
    let text = tokens_to_str(tokens.iter(), 0);

    fs::create_dir_all(ARTIFACT_DIR).unwrap();
    let filepath = format!("{ARTIFACT_DIR}/{filename}_tokens.txt");
    let write_result = fs::write(&filepath, text);
    if let Err(err) = write_result {
        return Err(ErrorQueue::new_single_diag(
            LLVMError::file_writing_error(
                filepath,
                "tokens".to_owned(),
                err.to_string()
            ).to_diagnostic()))
    }
    Ok(())
}

fn generate_ast_file(filename: &String, statements: &SyntacticResult) -> Result<(), ErrorQueue> {
    let text = statements.statements.iter().map(ToString::to_string).collect::<Vec<_>>().join("\n");

    fs::create_dir_all(ARTIFACT_DIR).unwrap();
    let filepath = format!("{ARTIFACT_DIR}/{filename}_AST.txt");
    let write_result = fs::write(&filepath, text);
    if let Err(err) = write_result {
        return Err(ErrorQueue::new_single_diag(LLVMError::file_writing_error(
            filepath, "unlinked AST".to_owned(), err.to_string()
        ).to_diagnostic()))
    }
    Ok(())
}

fn generate_last_file(filename: &String, linked_program: &LinkedProgram) -> Result<(), ErrorQueue> {
    let text = [&linked_program.extern_statements, &linked_program.type_statements, &linked_program.variable_statement, &linked_program.function_statement]
        .iter().map(|hashmap| hashmap.iter()
        .map(|(&object, statement)| statement.to_string::<true>(&linked_program.factory, object) + "\n")
        .collect::<String>()
    ).collect::<String>();

    fs::create_dir_all(ARTIFACT_DIR).unwrap();
    let filepath = format!("{ARTIFACT_DIR}/{filename}_LAST.txt");
    let write_result = fs::write(&filepath, text);
    if let Err(err) = write_result {
        return Err(ErrorQueue::new_single_diag(LLVMError::file_writing_error(
            filepath, "AST".to_owned(), err.to_string()
        ).to_diagnostic()))
    }
    Ok(())
}
