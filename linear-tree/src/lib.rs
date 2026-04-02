//! This crate allows to construct tree from left to right inside single Vec
//!
//! Due to limitations of train Iterator IntoIter can't implement it (it needs lifetime parameter of &mut self in return type to guarantee drop order)
//!
//! # Example
//!
//! ```rust
//! use linear_tree::{TreeBuilder, NodeRef, Node, NodeIntoIterator, build_no_meta};
//!
//! #[derive(Eq, PartialEq, Debug)]
//! struct Elem { data: usize }
//! #[derive(Eq, PartialEq, Debug)]
//! struct Block { data: usize }
//!
//! // or nodes: NodeIterator<'_, Elem, Block, ()>
//! fn collect<'a>(vec: &mut Vec<usize>, nodes: impl Iterator<Item=(NodeRef<'a, Elem, Block, ()>, &'a ())>) {
//!     for node in nodes {
//!         match node.0 {
//!             NodeRef::Elem(elem) => {
//!                 vec.push(elem.data);
//!             }
//!             NodeRef::Block(block, iter) => {
//!                 vec.push(block.data);
//!                 collect(vec, iter)
//!             }
//!         }
//!     }
//! }
//!
//! // or nodes: NodeIntoIterator<'_, Elem, Block, ()>
//! fn consume<'a>(vec: &mut Vec<usize>, nodes: &mut impl Iterator<Item=(Node<'a, Elem, Block, ()>, ())>) {
//!     loop {
//!         let Some(node) = nodes.next() else {
//!             return;
//!         };
//!         match node.0 {
//!             Node::Elem(elem) => {
//!                 vec.push(elem.data);
//!             }
//!             Node::Block(block, mut iter) => {
//!                 vec.push(block.data);
//!                 consume(vec, &mut iter);
//!             }
//!         }
//!     }
//! }
//!
//! let mut builder = TreeBuilder::<Elem, Block, ()>::new();
//! builder.add_elem(Elem { data: 1 }, ());
//! builder.start_new_block();
//!
//! builder.start_new_block();
//! builder.close_block(Block { data: 3 }, ());
//!
//! builder.add_elem(Elem { data: 4 }, ());
//! builder.close_block(Block { data: 2 }, ());
//! builder.add_elem(Elem { data: 5 }, ());
//!
//! let tree = builder.finish_building();
//! assert_eq!(tree, build_no_meta!(
//!     Elem { data: 1 },
//!     [ Block { data: 2 };
//!         [ Block { data: 3 };
//!         ]
//!         Elem { data: 4 },
//!     ]
//!     Elem { data: 5 },
//! ));
//!
//! let mut vec = Vec::new();
//! collect(&mut vec, tree.iter());
//! assert_eq!(vec, vec![1, 2, 3, 4, 5]);
//!
//! // consume tree
//! let mut vec = Vec::new();
//! let mut root_iter = tree.into_iter();
//! consume(&mut vec, &mut root_iter.iterator());
//! assert_eq!(vec, vec![1, 2, 3, 4, 5]);
//! ```

mod builder;
pub use builder::TreeBuilder;

mod macro_builder;

mod iterator;
pub use iterator::{NodeRef, NodeIterator};

mod into_iter;
pub use into_iter::{Node, NodeIntoIterator};

mod compare;

type NodeWithMeta<Elem, Block, Meta> = (TreeNode<Elem, Block>, Meta);

#[derive(Debug, Clone)]
pub struct LinearTree<Elem, Block, Meta> {
    tree: Vec<NodeWithMeta<Elem, Block, Meta>>
}

/// node stored inside tree
#[derive(Debug, Clone)]
enum TreeNode<Elem, Block> {
    Elem(Elem),
    Block(NodeBlock<Block>),
}

#[derive(Debug, Clone)]
struct NodeBlock<Block> {
    pub block: Block,
    start: usize,
    end: usize,
}
