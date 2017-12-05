pub mod node;
pub mod source;

use toolshed::list::{List, UnsafeList};
use toolshed::Arena;
use std::marker::PhantomData;

pub use ast::source::*;
pub use ast::node::{Node, NodeInner};

pub type NodeList<'ast, T> = List<'ast, Node<'ast, T>>;
pub type SourceUnitNode<'ast> = Node<'ast, SourceUnit<'ast>>;
pub type SourceUnitList<'ast> = NodeList<'ast, SourceUnit<'ast>>;


/// A Solidity source code parsed to an AST
pub struct Program<'ast> {
    body: UnsafeList,
    arena: Arena,
    _phantom: PhantomData<SourceUnitList<'ast>>
}

impl<'ast> Program<'ast> {
    #[inline]
    pub(crate) fn new(body: UnsafeList, arena: Arena) -> Self {
        Program {
            body,
            arena,
            _phantom: PhantomData,
        }
    }

    /// Get the list of `SourceUnit`s.
    #[inline]
    pub fn body(&self) -> SourceUnitList<'ast> {
        unsafe { self.body.into_list() }
    }

    /// Get a reference to the `Arena` on which the AST is allocated.
    #[inline]
    pub fn arena(&'ast self) -> &'ast Arena {
        &self.arena
    }
}
