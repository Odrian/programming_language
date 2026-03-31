
//! This crate allows to construct tree from left to right inside single Vec
//!
//! Due to limitations of train Iterator IntoIter can't implement it (it needs lifetime parameter of &mut self in return type to guarantee drop order)
//!
//! # Example
//!
//! ```rust
//! let mut construction = TreeConstructor::<Elem, Block>::new();
//! construction.add_elem(Elem::from(1));
//! construction.start_new_block(Block::from(2));
//!
//! construction.start_new_block(Block::from(3));
//! construction.close_block();
//!
//! construction.add_elem(Elem::from(4));
//! construction.close_block();
//! construction.add_elem(Elem::from(5));
//!
//! let tree = construction.finish_building();
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

mod construct;
pub use construct::TreeConstructor;

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
