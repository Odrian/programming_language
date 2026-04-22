use crate::{LinearTree, NodeWithMeta, TreeNode};
use std::marker::PhantomData;
use std::mem::ManuallyDrop;

/// node that "owns" itself and subnodes
pub enum Node<'inner, Elem, Block, Meta> {
    Elem(Elem),
    Block(Block, NodeIntoIterator<'inner, Elem, Block, Meta>)
}

impl<Elem, Block, Meta> LinearTree<Elem, Block, Meta> {
    pub fn into_iter(self) -> RootNodeIntoIterator<'static, Elem, Block, Meta> {
        unsafe {
            let mut me = ManuallyDrop::new(self);

            let ptr = me.tree.as_mut_ptr();
            let capacity = me.tree.capacity();

            let begin = ptr;
            let end = ptr.add(me.tree.len());

            RootNodeIntoIterator {
                iterator: ManuallyDrop::new(
                    NodeIntoIterator { ptr, begin, end, phantom: PhantomData }
                ),
                capacity,
            }
        }
    }
}

/// Safety: owns container and iterator elements
pub struct RootNodeIntoIterator<'a, Elem, Block, Meta> {
    iterator: ManuallyDrop<NodeIntoIterator<'a, Elem, Block, Meta>>,
    capacity: usize,
}

impl<'a, Elem, Block, Meta> RootNodeIntoIterator<'a, Elem, Block, Meta> {
    pub fn iterator(&mut self) -> &mut NodeIntoIterator<'a, Elem, Block, Meta> {
        &mut self.iterator
    }
}


/// Safety: owns memory in [begin, end), but not the container
pub struct NodeIntoIterator<'a, Elem, Block, Meta> {
    /// vector ptr
    ptr: *mut NodeWithMeta<Elem, Block, Meta>,
    /// next node
    begin: *mut NodeWithMeta<Elem, Block, Meta>,
    /// after last node, == ptr + length
    end: *mut NodeWithMeta<Elem, Block, Meta>,

    phantom: PhantomData<&'a [NodeWithMeta<Elem, Block, Meta>]>
}

impl<'outer, Elem, Block, Meta> NodeIntoIterator<'outer, Elem, Block, Meta> {
    pub fn is_empty(&self) -> bool {
        self.begin == self.end
    }
    pub fn len(&self) -> usize {
        unsafe { self.end.offset_from_unsigned(self.begin) }
    }
    pub fn peek(&self) -> Option<(Node<'outer, &'outer Elem, &'outer Block, &'outer Meta>, &'outer Meta)> {
        if self.begin == self.end {
            return None;
        }

        unsafe {
            let node = &*self.begin as &'outer NodeWithMeta<Elem, Block, Meta>;

            match &node.0 {
                TreeNode::Elem(elem) => {
                    Some((Node::Elem(elem), &node.1))
                }
                TreeNode::Block(node_block) => {
                    let ptr = self.ptr as *mut NodeWithMeta<&Elem, &Block, &Meta>;
                    let fake_iter = NodeIntoIterator { ptr, begin: ptr, end: ptr, phantom: PhantomData };

                    Some((Node::Block(
                        &node_block.block,
                        fake_iter,
                    ), &node.1))
                }
            }
        }
    }
}

impl<'outer, Elem, Block, Meta> Iterator for NodeIntoIterator<'outer, Elem, Block, Meta> {
    type Item = (Node<'outer, Elem, Block, Meta>, Meta);

    fn next(&mut self) -> Option<Self::Item> {
        if self.begin == self.end {
            return None;
        }

        unsafe {
            let node = std::ptr::read(self.begin);

            match node.0 {
                TreeNode::Elem(elem) => {
                    self.begin = self.begin.add(1);

                    Some((Node::Elem(elem), node.1))
                }
                TreeNode::Block(node_block) => {
                    let begin = self.ptr.add(node_block.start);
                    let end = self.ptr.add(node_block.end);
                    let iter = NodeIntoIterator { ptr: self.ptr, begin, end, phantom: PhantomData };

                    self.begin = end;
                    Some((Node::Block(
                        node_block.block,
                        iter,
                    ), node.1))
                }
            }
        }
    }
}

impl<Enum, Block, Meta> Drop for NodeIntoIterator<'_, Enum, Block, Meta> {
    fn drop(&mut self) {
        unsafe {
            while self.begin != self.end {
                std::ptr::drop_in_place(self.begin);
                self.begin = self.begin.add(1);
            }
        }
    }
}

impl<Elem, Block, Meta> Drop for RootNodeIntoIterator<'_, Elem, Block, Meta> {
    fn drop(&mut self) {
        unsafe {
            let ptr = self.iterator.ptr;
            ManuallyDrop::drop(&mut self.iterator);

            let _ = Vec::<NodeWithMeta<Elem, Block, Meta>>::from_raw_parts(
                ptr,
                0,
                self.capacity
            );
        }
    }
}
