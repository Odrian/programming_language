pub mod token;

mod parser_tokens;
mod error;

use pr_common::error::ErrorQueue;
use token::{Token, TokenBlock};

type TokenTreeBuilder = pr_common::ranged_tree::TreeBuilder<Token, TokenBlock>;

pub type TokenLinearTree = pr_common::ranged_tree::LinearTree<Token, TokenBlock>;
pub type TokenIntoIter<'a> = pr_common::ranged_tree::NodeIntoIter<'a, Token, TokenBlock>;
pub type TokenIter<'a> = pr_common::ranged_tree::NodeIterator<'a, Token, TokenBlock>;

pub fn tokenize(errors: &mut ErrorQueue, text: &str) -> TokenLinearTree {
    parser_tokens::parse_tokens(errors, text)
}
