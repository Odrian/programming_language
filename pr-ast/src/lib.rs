pub mod statement;

mod parser_statements;
mod parse_expression;
mod parse_attributes;
mod parse_import;
mod parse_function;
mod parse_extern;
mod parse_struct;
mod parse_type;

mod error;

use crate::error::SyntacticError;
use lsp_types::Range;
use pr_common::error::{Diagnostic, ErrorQueue};
use pr_common::Target;
use pr_lexer::token::{Token, TokenBlock};
use pr_lexer::{TokenIntoIter, TokenLinearTree};
use statement::RStatement;

pub struct SyntacticResult {
    pub statements: Vec<RStatement>
}

pub fn parse_ast(errors: &mut ErrorQueue, target: &Target, tokens: TokenLinearTree) -> SyntacticResult {
    let unused_range = Range::default();
    let mut iter = tokens.into_iter();
    let statements = ParsingState::new(errors, target, iter.iterator(), unused_range).parse_statements_inplace(true);
    SyntacticResult { statements }
}

type Node<'a> = pr_common::ranged_tree::Node<'a, Token, TokenBlock>;
type RefNode<'a> = pr_common::ranged_tree::RefNode<'a, Token, TokenBlock>;
type RangedToken<'a> = (Node<'a>, Range);
type RefRangedToken<'a> = (RefNode<'a>, &'a Range);

struct ParsingState<'e, 'a, 'b> {
    errors: &'e mut ErrorQueue,
    target: &'e Target,
    tokens: &'a mut TokenIntoIter<'b>,
    all_range: Range,
}
impl<'e, 'a, 'b> ParsingState<'e, 'a, 'b> {
    pub fn new<'e2, 'a2, 'b2>(errors: &'e2 mut ErrorQueue, target: &'e2 Target, tokens: &'a2 mut TokenIntoIter<'b2>, all_range: Range) -> ParsingState<'e2, 'a2, 'b2> {
        ParsingState { errors, target, tokens, all_range }
    }
    pub fn new_state<'e2, 'a2, 'b2>(&'e2 mut self, tokens: &'a2 mut TokenIntoIter<'b2>, all_range: Range) -> ParsingState<'e2, 'a2, 'b2> {
        ParsingState {
            errors: self.errors,
            target: self.target,
            tokens, all_range
        }
    }
    pub fn add_diag(&mut self, diagnostic: Diagnostic) {
        self.errors.add_diag(diagnostic)
    }
    #[must_use]
    pub fn next(&mut self) -> Option<RangedToken<'b>> {
        self.tokens.next()
    }
    pub fn next_unwrap(&mut self, gen_diag: impl Fn(&mut ParsingState<'e, 'a, 'b>) -> Diagnostic) -> Result<RangedToken<'a>, ()> {
        let Some((token, range)) = self.tokens.next() else {
            let diag = gen_diag(self);
            self.errors.add_diag(diag);
            return Err(())
        };
        Ok((token, range))
    }

    pub fn peek(&self) -> Option<RefRangedToken<'b>> {
        self.tokens.peek()
    }
    pub fn at_end(&self) -> bool {
        self.tokens.is_empty()
    }
    pub fn consume_semicolon(&mut self, range_before: Range) {
        let Some((token, _range)) = self.peek() else {
            return;
        };

        if !matches!(token, RefNode::Elem(Token::Semicolon)) {
            self.add_diag(SyntacticError::expected_semicolon(range_before));
            return;
        }

        let _ = self.next();
    }
}
