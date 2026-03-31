use crate::{LinearTree, TreeNode};
use std::marker::PhantomData;
use std::mem::ManuallyDrop;

/// node that "owns" itself and subnodes
pub enum Node<T, Elem, Block> {
    Elem(Elem),
    Block(Block, SubNodeIntoIterator<T, Elem, Block>)
}

impl<Elem, Block> LinearTree<Elem, Block> {
    pub fn into_iter(self) -> NodeIntoIterator<(), Elem, Block> {
        unsafe {
            let mut me = ManuallyDrop::new(self);

            let ptr = me.tree.as_mut_ptr();
            let capacity = me.tree.capacity();

            let begin = ptr;
            let end = ptr.add(me.tree.len());

            NodeIntoIterator::<(), Elem, Block> {
                iterator: ManuallyDrop::new(
                    SubNodeIntoIterator { ptr, begin, end, phantom: PhantomData }
                ),
                capacity,
            }
        }
    }
}

/// Safety: owns container and iterator elements
pub struct NodeIntoIterator<T, Elem, Block> {
    iterator: ManuallyDrop<SubNodeIntoIterator<T, Elem, Block>>,
    capacity: usize,
}

impl<T, Elem, Block> NodeIntoIterator<T, Elem, Block> {
    pub fn next(&mut self) -> Option<Node<&T, Elem, Block>> {
        self.iterator.next()
    }
}


/// Safety: owns memory in [begin, end), but not the container
pub struct SubNodeIntoIterator<T, Elem, Block> {
    /// vector ptr
    ptr: *mut TreeNode<Elem, Block>,
    /// next node
    begin: *mut TreeNode<Elem, Block>,
    /// after last node, == ptr + length
    end: *mut TreeNode<Elem, Block>,

    phantom: PhantomData<T>
}

impl<T, Elem, Block> SubNodeIntoIterator<T, Elem, Block> {
    pub fn next(&mut self) -> Option<Node<&T, Elem, Block>> {
        if self.begin == self.end {
            return None;
        }

        unsafe {
            let node = std::ptr::read(self.begin); // error

            match node {
                TreeNode::Elem(elem) => {
                    self.begin = self.begin.add(1);

                    Some(Node::Elem(elem))
                }
                TreeNode::Block(node_block) => {
                    let begin = self.ptr.add(node_block.start);
                    let end = self.ptr.add(node_block.end);
                    let iter = SubNodeIntoIterator { ptr: self.ptr, begin, end, phantom: PhantomData };

                    self.begin = end;
                    Some(Node::Block(
                        node_block.block,
                        iter,
                    ))
                }
            }
        }
    }
}

impl<T, Enum, Block> Drop for SubNodeIntoIterator<T, Enum, Block> {
    fn drop(&mut self) {
        unsafe {
            while self.begin != self.end {
                std::ptr::drop_in_place(self.begin);
                self.begin = self.begin.add(1);
            }
        }
    }
}

impl<T, Elem, Block> Drop for NodeIntoIterator<T, Elem, Block> {
    fn drop(&mut self) {
        unsafe {
            let ptr = self.iterator.ptr;
            ManuallyDrop::drop(&mut self.iterator);

            let _ = Vec::<TreeNode<Elem, Block>>::from_raw_parts(
                ptr,
                0,
                self.capacity
            );
        }
    }
}
