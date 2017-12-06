use ast::{Node, NodeInner, NodeList};
use toolshed::Arena;

pub struct Mock {
    arena: Arena
}

impl Mock {
    pub fn new() -> Self {
        Mock {
            arena: Arena::new()
        }
    }

    pub fn node<'a, T, I, R>(&'a self, start: u32, end: u32, val: I) -> R
    where
        T: 'a + Copy,
        I: Into<T>,
        R: From<Node<'a, T>>,
    {
        Node::new(self.arena.alloc(NodeInner::new(start, end, val.into()))).into()
    }

    pub fn list<'a, T, L>(&'a self, list: L) -> NodeList<'a, T> where
        T: 'a + Copy,
        L: AsRef<[Node<'a, T>]>,
    {
        NodeList::from_iter(&self.arena, list.as_ref().iter().cloned())
    }
}
