use crate::{LinearTree, TreeNode, NodeBlock};

#[derive(Clone)]
pub struct TreeBuilder<Elem, Block> {
    tree: Vec<TreeNode<Elem, Block>>,
    start: usize,

    steps: Vec<BlockStart>,
}

#[derive(Copy, Clone)]
struct BlockStart {
    start: usize,
}

impl<Elem, Block> Default for TreeBuilder<Elem, Block> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Elem, Block> TreeBuilder<Elem, Block> {
    pub fn new() -> Self {
        Self { tree: Vec::default(), start: 0, steps: Vec::default()}
    }
    pub fn with_capacity(capacity: usize) -> Self {
        Self { tree: Vec::with_capacity(capacity), start: 0, steps: Vec::default() }
    }
    pub fn start_new_block(&mut self, block: Block) {
        self.steps.push(BlockStart { start: self.start });

        let start = self.tree.len();
        self.tree.push(TreeNode::Block(NodeBlock::new_uninit(block, start)));
        self.start = start;
    }
    pub fn close_block(&mut self) {
        let Some(block) = self.steps.pop() else {
            panic!("closing global block")
        };

        let actual_end = self.tree.len();
        let TreeNode::Block(node_block) = &mut self.tree[self.start] else { unreachable!() };
        node_block.init(actual_end);

        self.start = block.start;
    }
    pub fn add_elem(&mut self, elem: Elem) {
        self.tree.push(TreeNode::Elem(elem))
    }
    pub fn finish_building(self) -> LinearTree<Elem, Block> {
        if !self.steps.is_empty() {
            panic!("finish before stepping out")
        }
        LinearTree {
            tree: self.tree
        }
    }
}

impl<Block> NodeBlock<Block> {
    fn new_uninit(block: Block, start: usize) -> Self {
        Self {
            block,
            start,
            end: usize::MAX,
        }
    }
    fn init(&mut self, end: usize) {
        assert_eq!(self.end, usize::MAX);
        self.start += 1; // move start to actual start of block
        self.end = end;
    }
}
