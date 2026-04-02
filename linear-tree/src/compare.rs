use crate::{LinearTree, NodeRef};
use crate::iterator::NodeIterator;

impl<Elem: Eq, Block: Eq, Meta: Eq> Eq for LinearTree<Elem, Block, Meta> {}
impl<Elem: PartialEq, Block: PartialEq, Meta: PartialEq> PartialEq for LinearTree<Elem, Block, Meta> {
    fn eq(&self, other: &Self) -> bool {
        self.iter().eq(other.iter())
    }
}

impl<Elem: PartialEq, Block: PartialEq, Meta: PartialEq> NodeIterator<'_, Elem, Block, Meta> {
    pub fn eq(mut self, mut other: Self) -> bool {
        loop {
            match (self.next(), other.next()) {
                (Some((node1, meta1)), Some((node2, meta2))) => {
                    if !(node1.eq(node2) && meta1 == meta2) {
                        return false;
                    }
                }
                (None, None) => return true,
                _ => return false,
            }
        }
    }
}

impl<Elem: PartialEq, Block: PartialEq, Meta: PartialEq> NodeRef<'_, Elem, Block, Meta> {
    pub fn eq(self, other: Self) -> bool {
        match (self, other) {
            (NodeRef::Elem(elem1), NodeRef::Elem(elem2)) =>
                elem1 == elem2,
            (NodeRef::Block(block1, iter1), NodeRef::Block(block2, iter2)) =>
                block1 == block2 && iter1.eq(iter2),
            _ => false
        }
    }
}


impl<Elem: PartialEq, Block: PartialEq, Meta> LinearTree<Elem, Block, Meta> {
    pub fn eq_without_meta(&self, other: &Self) -> bool {
        self.iter().eq_without_meta(other.iter())
    }
}

impl<Elem: PartialEq, Block: PartialEq, Meta> NodeIterator<'_, Elem, Block, Meta> {
    pub fn eq_without_meta(mut self, mut other: Self) -> bool {
        loop {
            match (self.next(), other.next()) {
                (Some((node1, _meta1)), Some((node2, _meta2))) => {
                    if !node1.eq_without_meta(node2) {
                        return false;
                    }
                }
                (None, None) => return true,
                _ => return false,
            }
        }
    }
}

impl<Elem: PartialEq, Block: PartialEq, Meta> NodeRef<'_, Elem, Block, Meta> {
    pub fn eq_without_meta(self, other: Self) -> bool {
        match (self, other) {
            (NodeRef::Elem(elem1), NodeRef::Elem(elem2)) =>
                elem1 == elem2,
            (NodeRef::Block(block1, iter1), NodeRef::Block(block2, iter2)) =>
                block1 == block2 && iter1.eq_without_meta(iter2),
            _ => false
        }
    }
}
