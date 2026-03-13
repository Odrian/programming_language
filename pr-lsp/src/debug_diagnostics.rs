use pr_core::parser::parse1_tokenize::token::{RangedToken, Token};
use pr_core::parser::parse2_syntactic::statement::*;
use pr_core::Ranged;
use std::fmt::Debug;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Range};

#[allow(dead_code)]
pub fn token_diag(tokens: &Vec<RangedToken>) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    let mut debug = DebugDiagnostics::new(&mut diagnostics);
    debug.token_diags(tokens);
    diagnostics
}

#[allow(dead_code)]
pub fn statement_diag(statements: &Vec<RStatement>) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    let mut debug = DebugDiagnostics::new(&mut diagnostics);
    debug.statements_diags(statements);
    diagnostics
}

struct DebugDiagnostics<'a> {
    diagnostics: &'a mut Vec<Diagnostic>
}

impl DebugDiagnostics<'_> {
    fn new(diagnostics: &mut Vec<Diagnostic>) -> DebugDiagnostics<'_> {
        DebugDiagnostics { diagnostics }
    }
    fn hint_diag(&mut self, text: String, range: Range) {
        self.diagnostics.push(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::HINT),
            message: text,
            ..Default::default()
        })
    }

    fn hint_ranged<T: Debug>(&mut self, ranged: &Ranged<T>) {
        self.hint_diag(format!("{:?}", ranged.value), ranged.range)
    }
    fn hint_ranged_option<T: Debug>(&mut self, ranged: &Option<Ranged<T>>) {
        ranged.as_ref().map(|r| self.hint_ranged(r));
    }

    /// add hints to all tokens
    fn token_diags(&mut self, tokens: &Vec<RangedToken>) {
        for RangedToken { token, range } in tokens {
            if let Token::Bracket(body, bracket) = token {
                let mut start1 = range.start;
                start1.character += 1;
                let mut end1 = range.end;
                end1.character -= 1;
                self.hint_diag(
                    bracket.to_open_string().to_string(),
                    Range::new(range.start, start1));
                self.hint_diag(
                    bracket.to_close_string().to_string(),
                    Range::new(end1, range.end));
                self.token_diags(body)
            } else {
                self.hint_diag(format!("{token:?}"), *range);
            }
        }
    }

    fn statements_diags(&mut self, statements: &Vec<RStatement>) {
        for RStatement { value: statement, range } in statements {
            match statement {
                Statement::ComptimeStatement(comp) => match comp {
                    ComptimeStatement::Import { from, what } => {
                        from.iter().for_each(|s| self.hint_ranged(s));
                        what.iter().for_each(|(l, r)| {
                            self.hint_ranged(l);
                            self.hint_ranged_option(r);
                        })
                    }
                }
                Statement::DeclarationStatement { name, statement } => {
                    self.hint_ranged(name);
                    match statement {
                        DeclarationStatement::VariableDeclaration { typee, value } => {
                            self.typee_diag_option(typee);
                            self.hint_ranged_option(typee);
                            self.expr_diags(value);
                        }
                        DeclarationStatement::Function { args, returns, body } => {
                            args.iter().for_each(|(name, typee)| {
                                self.hint_ranged(name);
                                self.typee_diag(typee);
                            });
                            self.typee_diag_option(returns);
                            self.statements_diags(body);
                        }
                        DeclarationStatement::Struct { fields } =>
                            fields.iter().for_each(|(name, typee)| {
                                self.hint_ranged(name);
                                self.typee_diag(typee);
                            }),
                    }
                }
                Statement::ExternStatement { statement: ext } => match ext {
                    ExternStatement::Variable { name, typee } => {
                        self.hint_ranged(name);
                        self.typee_diag(typee);
                    }
                    ExternStatement::Function { name, args, is_vararg: _, returns } => {
                        self.hint_ranged(name);
                        args.iter().for_each(|typee| self.typee_diag(typee));
                        self.typee_diag_option(returns);
                    }
                }

                Statement::SetVariable { what, value, op } => {
                    self.expr_diags(what);
                    self.expr_diags(value);
                    self.hint_ranged_option(op);
                }

                Statement::Brackets(body) =>
                    self.statements_diags(body),
                Statement::Expression(expr) =>
                    self.expr_diags(&expr.clone().add_range(*range)),
                Statement::If { condition, body } => {
                    self.statements_diags(body);
                    self.expr_diags(condition)
                }
                Statement::While { condition, body } => {
                    self.statements_diags(body);
                    self.expr_diags(condition)
                }
                Statement::Return(returns) =>
                    returns.iter().for_each(|r| self.expr_diags(r)),
            }
        }
    }

    fn expr_diags(&mut self, expr: &RExpression) {
        match &expr.value {
            Expression::Operation(left, right, op) => {
                self.expr_diags(left);
                self.expr_diags(right);
                self.hint_ranged(op);
            }
            Expression::UnaryOperation(expr, op) => {
                self.expr_diags(expr);
                self.hint_ranged(op);
            }
            Expression::As(expr, typee) => {
                self.expr_diags(expr);
                self.typee_diag(typee);
            }
            Expression::StructField { left, field } => {
                self.expr_diags(left);
                self.hint_ranged(field);
            }

            Expression::Literal(_) =>
                self.hint_ranged(expr),
            Expression::StructConstruct { struct_name, fields } => {
                self.hint_ranged(struct_name);
                fields.iter().for_each(|(name, value)| {
                    self.hint_ranged(name);
                    self.expr_diags(value);
                });
            }

            Expression::Variable(str) =>
                self.hint_ranged(str),
            Expression::RoundBracket(expr) =>
                self.expr_diags(expr),
            Expression::FunctionCall { object, args } => {
                self.hint_ranged(object);
                args.iter().for_each(|e| self.expr_diags(e));
            }
        }
    }

    fn typee_diag(&mut self, typee: &RTypee) {
        self.hint_diag(typee.value.to_string(), typee.range)
    }
    fn typee_diag_option(&mut self, typee: &Option<RTypee>) {
        typee.iter().for_each(|typee| self.typee_diag(typee));
    }
}