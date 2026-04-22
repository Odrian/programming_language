use crate::{LinearTree, TreeNode, NodeBlock, NodeWithMeta};

/// node that returned by iterator
#[derive(Clone)]
pub enum NodeRef<'a, Elem, Block, Meta> {
    Elem(&'a Elem),
    Block(&'a Block, NodeIterator<'a, Elem, Block, Meta>),
}

impl<Elem, Block, Meta> LinearTree<Elem, Block, Meta> {
    pub fn iter(&'_ self) -> NodeIterator<'_, Elem, Block, Meta> {
        NodeIterator::new(self)
    }
}

impl<'a, Elem, Block, Meta> IntoIterator for &'a LinearTree<Elem, Block, Meta> {
    type Item = (NodeRef<'a, Elem, Block, Meta>, &'a Meta);
    type IntoIter = NodeIterator<'a, Elem, Block, Meta>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

#[derive(Copy, Clone)]
pub struct NodeIterator<'a, Elem, Block, Meta> {
    tree: &'a Vec<NodeWithMeta<Elem, Block, Meta>>,
    start: usize,
    end: usize,
}

impl<'a, Elem, Block, Meta> NodeIterator<'a, Elem, Block, Meta> {
    fn new(linear_tree: &'a LinearTree<Elem, Block, Meta>) -> Self {
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

impl<'a, Elem, Block, Meta> Iterator for NodeIterator<'a, Elem, Block, Meta> {
    type Item = (NodeRef<'a, Elem, Block, Meta>, &'a Meta);

    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        if self.start == self.end {
            return None;
        }

        let node = &self.tree[self.start];

        match &node.0 {
            TreeNode::Elem(elem) => {
                self.start += 1;
                Some((NodeRef::Elem(elem), &node.1))
            }
            TreeNode::Block(block_data) => {
                self.start = block_data.end;
                Some((NodeRef::Block(
                    &block_data.block,
                    self.new2(block_data)
                ), &node.1))
            }
        }
    }
}
