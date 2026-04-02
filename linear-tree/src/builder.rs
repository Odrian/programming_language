use std::mem::MaybeUninit;
use crate::{LinearTree, TreeNode, NodeBlock, NodeWithMeta};

pub struct TreeBuilder<Elem, Block, Meta> {
    tree: Vec<MaybeUninit<NodeWithMeta<Elem, Block, Meta>>>,
    start: usize,

    steps: Vec<BlockStart>,
}

struct BlockStart {
    start: usize,
}

impl<Elem, Block, Meta> Default for TreeBuilder<Elem, Block, Meta> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Elem, Block, Meta> TreeBuilder<Elem, Block, Meta> {
    pub fn new() -> Self {
        Self { tree: Vec::default(), start: 0, steps: Vec::default()}
    }
    pub fn with_capacity(capacity: usize) -> Self {
        Self { tree: Vec::with_capacity(capacity), start: 0, steps: Vec::default() }
    }
    pub fn start_new_block(&mut self) {
        self.steps.push(BlockStart { start: self.start });

        let start = self.tree.len();
        self.tree.push(MaybeUninit::uninit());
        self.start = start;
    }
    pub fn close_block(&mut self, block: Block, meta: Meta) {
        let Some(block_start) = self.steps.pop() else {
            panic!("closing global block")
        };

        let end = self.tree.len();
        let node: &mut MaybeUninit<_> = &mut self.tree[self.start];
        node.write((
            TreeNode::Block(NodeBlock { block, start: self.start + 1, end }),
            meta
        ));

        self.start = block_start.start;
    }
    pub fn add_elem(&mut self, elem: Elem, meta: Meta) {
        self.tree.push(MaybeUninit::new((TreeNode::Elem(elem), meta)))
    }
    pub fn finish_building(self) -> LinearTree<Elem, Block, Meta> {
        if !self.steps.is_empty() {
            panic!("finish before stepping out")
        }
        // Safety: only unfinished blocks uninit, self.steps is empty, so all init
        LinearTree {
            tree: unsafe { std::mem::transmute(self.tree) }
        }
    }
}
