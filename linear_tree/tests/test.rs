use linear_tree::{LinearTree, Node, NodeRef, TreeBuilder};

struct Elem {
    data: usize,
    _ptr: Box<usize>,
}

impl Elem {
    fn from(data: usize) -> Self { Self { data, _ptr: Box::new(1) } }
}

struct Block {
    data: usize,
    _ptr: Box<usize>,
}
impl Block {
    fn from(data: usize) -> Self { Self { data, _ptr: Box::new(1) } }
}

struct TreeCollector {
    vec: Vec<usize>
}
impl TreeCollector {
    fn consume(tree: &LinearTree<Elem, Block>) -> Vec<usize> {
        let mut self0 = Self { vec: Vec::default() };
        self0.collect(tree.iter());
        self0.vec
    }
    fn collect<'a>(&mut self, nodes: impl Iterator<Item=NodeRef<'a, Elem, Block>>) {
        for node in nodes {
            match node {
                NodeRef::Elem(elem) => {
                    self.vec.push(elem.data);
                }
                NodeRef::Block(block, iter) => {
                    self.vec.push(block.data);
                    self.collect(iter)
                }
            }
        }
    }
}

#[test]
fn test_iter() {
    let mut construction = TreeBuilder::<Elem, Block>::new();

    construction.add_elem(Elem::from(0));
    construction.add_elem(Elem::from(1));
    construction.start_new_block(Block::from(2));
    construction.add_elem(Elem::from(3));
    construction.close_block();
    construction.add_elem(Elem::from(4));

    let tree = construction.finish_building();

    let data = TreeCollector::consume(&tree);

    assert_eq!(data, vec![0, 1, 2, 3, 4])
}

#[test]
fn test_into_iter() {
    let mut construction = TreeBuilder::<Elem, Block>::new();

    construction.add_elem(Elem::from(3));
    construction.start_new_block(Block::from(2));
    construction.add_elem(Elem::from(3));
    construction.start_new_block(Block::from(2));
    construction.add_elem(Elem::from(3));
    construction.add_elem(Elem::from(3));
    construction.close_block();
    construction.add_elem(Elem::from(3));
    construction.close_block();
    construction.add_elem(Elem::from(3));
    construction.add_elem(Elem::from(3));

    let tree = construction.finish_building();

    let mut iter = tree.into_iter();
    let _ = iter.next().unwrap();
    let Node::Block(_block, mut sub_iter) = iter.next().unwrap() else { unreachable!() };
    let _ = sub_iter.next().unwrap();
    let Node::Block(_block, mut sub_iter2) = sub_iter.next().unwrap() else { unreachable!() };
    let _ = sub_iter2.next().unwrap();
    drop(sub_iter2);
    drop(sub_iter);
    let _ = iter.next().unwrap();
}
