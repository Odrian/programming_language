//! This crate allows to construct tree from left to right inside single Vec
//!
//! Due to limitations of train Iterator IntoIter can't implement it (it needs lifetime parameter of &mut self in return type to guarantee drop order)
//!
//! # Example
//!
//! ```rust
//! use linear_tree::{TreeBuilder, NodeRef, Node};
//!
//! struct Elem { data: usize }
//! struct Block { data: usize }
//!
//! let mut builder = TreeBuilder::<Elem, Block>::new();
//! builder.add_elem(Elem { data: 1 });
//! builder.start_new_block(Block { data: 2 });
//!
//! builder.start_new_block(Block { data: 3 });
//! builder.close_block();
//!
//! builder.add_elem(Elem { data: 4 });
//! builder.close_block();
//! builder.add_elem(Elem { data: 5 });
//!
//! let tree = builder.finish_building();
//!
//! let mut vec = Vec::new();
//! for node in &tree {
//!     match node {
//!         NodeRef::Elem(elem) => {
//!             vec.push(elem.data);
//!         }
//!         NodeRef::Block(block, _sub_iter) => {
//!             vec.push(block.data);
//!         }
//!     }
//! }
//! assert_eq!(vec, vec![1, 2, 5]);
//!
//! // consume tree
//! let mut into_iter = tree.into_iter();
//! while let Some(node) = into_iter.next() {
//!     match node {
//!         Node::Elem(elem) => {
//!             // ...
//!         }
//!         Node::Block(block, sub_iter) => {
//!             // ...
//!         }
//!     }
//! }
//! ```

mod builder;
pub use builder::TreeBuilder;

mod iterator;
pub use iterator::NodeRef;

mod into_iter;
pub use into_iter::Node;

#[derive(Clone)]
pub struct LinearTree<Elem, Block> {
    tree: Vec<TreeNode<Elem, Block>>
}

/// node stored inside tree
#[derive(Clone)]
enum TreeNode<Elem, Block> {
    Elem(Elem),
    Block(NodeBlock<Block>),
}

#[derive(Clone)]
struct NodeBlock<Block> {
    pub block: Block,
    start: usize,
    end: usize,
}
