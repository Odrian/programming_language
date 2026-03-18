pub mod statement;

mod parser_statements;

mod error;

use pr_common::error::ErrorQueue;
use pr_tokenize::TokenizeResult;
use statement::RStatement;

pub struct SyntacticResult {
    pub statements: Vec<RStatement>
}

pub fn parse_ast(errors: &mut ErrorQueue, tokens: TokenizeResult) -> SyntacticResult {
    let statements = parser_statements::parse_statements(errors, tokens.tokens);
    SyntacticResult { statements }
}
