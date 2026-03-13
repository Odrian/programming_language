use std::fs;
use std::path::PathBuf;
use clap::Parser;
use pr_core::error::{range_to_str, ErrorQueue};
use pr_core::io_error::FileError;
use pr_core::parser::*;
use pr_core::parser::parse1_tokenize::token::{RangedToken, Token};
use pr_core::parser::parse2_syntactic::statement::RStatement;
use pr_core::parser::parse3_linking::LinkedProgram;

pub mod compiling;

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
    let text = fs::read_to_string(file_path).expect("can't read file");
    let mut errors = ErrorQueue::default();

    let tokens = parse1_tokenize::tokenize(&mut errors, &text);
    if args.gen_tokens { generate_tokens_file(&filename, &tokens)? }

    let statements = parse2_syntactic::parse_statements(&mut errors, tokens);
    if args.gen_ast { generate_ast_file(&filename, &statements)? }

    if errors.has_errors() {
        return Err(errors)
    }

    let linked_program = parse3_linking::link_all(statements)
        .map_err(|_| ErrorQueue::new_single_error("linking error"))?;
    if args.gen_last { generate_last_file(&filename, &linked_program)? }

    compiling::parse_to_llvm(args, linked_program)
        .map_err(|err| ErrorQueue::new_single_error(&err.to_string()))?;

    Ok(())
}

fn tokens_to_str(tokens: &Vec<RangedToken>, layer: u8) -> String {
    tokens.iter().map(|t| {
        let range = range_to_str(t.range);
        let str = match &t.token {
            Token::Bracket(body, bracket) => {
                let body = tokens_to_str(body, layer + 1);
                let left = bracket.to_open_string();
                let right = bracket.to_close_string();
                let spaces = "    ".repeat(layer as usize);
                format!("{left} {range}\n{body}\n{spaces}{right}")
            }
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
        };
        "    ".repeat(layer as usize).to_owned() + &str
    }).collect::<Vec<_>>().join("\n")
}

fn generate_tokens_file(filename: &String, tokens: &Vec<RangedToken>) -> Result<(), ErrorQueue> {
    let text = tokens_to_str(tokens, 0);

    fs::create_dir_all(ARTIFACT_DIR).unwrap();
    let filepath = format!("{ARTIFACT_DIR}/{filename}_tokens.txt");
    let write_result = fs::write(&filepath, text);
    if let Err(err) = write_result {
        FileError::CantWriteToFile {
            filepath,
            what: "tokens".to_owned(),
            io_error: err.to_string()
        }.print();
        return Err(ErrorQueue::new_single_error("file error"))
    }
    Ok(())
}

fn generate_ast_file(filename: &String, statements: &Vec<RStatement>) -> Result<(), ErrorQueue> {
    let text = statements.iter().map(ToString::to_string).collect::<Vec<_>>().join("\n");

    fs::create_dir_all(ARTIFACT_DIR).unwrap();
    let filepath = format!("{ARTIFACT_DIR}/{filename}_AST.txt");
    let write_result = fs::write(&filepath, text);
    if let Err(err) = write_result {
        FileError::CantWriteToFile {
            filepath,
            what: "unlinked AST".to_owned(),
            io_error: err.to_string()
        }.print();
        return Err(ErrorQueue::new_single_error("file error"))
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
        FileError::CantWriteToFile {
            filepath,
            what: "AST".to_owned(),
            io_error: err.to_string()
        }.print();
        return Err(ErrorQueue::new_single_error("file error"));
    }
    Ok(())
}
