use lsp_types::Range;
use linear_tree;

pub use linear_tree::{build, build_no_meta};


pub type NodeIntoIter<'a, Elem, Block> = linear_tree::NodeIntoIterator<'a, Elem, Block, Range>;
pub type NodeIterator<'a, Elem, Block> = linear_tree::NodeIterator<'a, Elem, Block, Range>;
pub type Node<'a, Elem, Block> = linear_tree::Node<'a, Elem, Block, Range>;
pub type RefNode<'a, Elem, Block> = linear_tree::Node<'a, &'a Elem, &'a Block, &'a Range>;
pub type NodeRef<'a, Elem, Block> = linear_tree::NodeRef<'a, Elem, Block, Range>;
pub type LinearTree<Elem, Block> = linear_tree::LinearTree<Elem, Block, Range>;
pub type TreeBuilder<Elem, Block> = linear_tree::TreeBuilder<Elem, Block, Range>;
