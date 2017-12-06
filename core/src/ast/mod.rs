#[macro_use]
mod impl_from;
mod node;
mod source;
mod contract;

use toolshed::list::{List, UnsafeList};
use toolshed::Arena;
use std::marker::PhantomData;

pub use ast::source::*;
pub use ast::contract::*;
pub use ast::node::{Node, NodeInner};

pub type Identifier<'ast> = &'ast str;
pub type StringLiteral<'ast> = &'ast str;
pub type VersionLiteral<'ast> = &'ast str;

pub type NodeList<'ast, T> = List<'ast, Node<'ast, T>>;
pub type SourceUnitNode<'ast> = Node<'ast, SourceUnit<'ast>>;
pub type SourceUnitList<'ast> = NodeList<'ast, SourceUnit<'ast>>;
pub type IdentifierNode<'ast> = Node<'ast, Identifier<'ast>>;
pub type IdentifierList<'ast> = NodeList<'ast, Identifier<'ast>>;


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
