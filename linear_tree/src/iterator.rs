use crate::{LinearTree, TreeNode, NodeBlock};

/// node that returned by iterator
#[derive(Clone)]
pub enum NodeRef<'a, Elem, Block> {
    Elem(&'a Elem),
    Block(&'a Block, NodeIterator<'a, Elem, Block>),
}

impl<Elem, Block> LinearTree<Elem, Block> {
    pub fn iter(&'_ self) -> NodeIterator<'_, Elem, Block> {
        NodeIterator::new(self)
    }
}

impl<'a, Elem, Block> IntoIterator for &'a LinearTree<Elem, Block> {
    type Item = NodeRef<'a, Elem, Block>;
    type IntoIter = NodeIterator<'a, Elem, Block>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

#[derive(Clone)]
pub struct NodeIterator<'a, Elem, Block> {
    tree: &'a Vec<TreeNode<Elem, Block>>,
    start: usize,
    end: usize,
}

impl<'a, Elem, Block> NodeIterator<'a, Elem, Block> {
    fn new(linear_tree: &'a LinearTree<Elem, Block>) -> Self {
        Self {
            tree: &linear_tree.tree,
            start: 0,
            end: linear_tree.tree.len()
        }
    }
    fn new2(&self, block_data: &NodeBlock<Block>) -> Self {
        Self {
            tree: self.tree,
            start: block_data.start,
            end: block_data.end,
        }
    }
}

impl<'a, Elem, Block> Iterator for NodeIterator<'a, Elem, Block> {
    type Item = NodeRef<'a, Elem, Block>;

    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        if self.start == self.end {
            return None;
        }

        let node = &self.tree[self.start];

        match node {
            TreeNode::Elem(elem) => {
                self.start += 1;
                Some(NodeRef::Elem(elem))
            }
            TreeNode::Block(block_data) => {
                self.start = block_data.end;
                Some(NodeRef::Block(
                    &block_data.block,
                    self.new2(block_data)
                ))
            }
        }
    }
}
