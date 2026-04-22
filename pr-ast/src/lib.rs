pub mod statement;

mod parser_statements;

mod error;

use pr_common::error::ErrorQueue;
use pr_common::Target;
use pr_lexer::TokenLinearTree;
use statement::RStatement;

pub struct SyntacticResult {
    pub statements: Vec<RStatement>
}

pub fn parse_ast(errors: &mut ErrorQueue, target: &Target, tokens: TokenLinearTree) -> SyntacticResult {
    let statements = parser_statements::parse_statements(errors, target, tokens);
    SyntacticResult { statements }
}
