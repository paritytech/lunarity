use ast::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Statement<'ast> {
    BlockStatement(BlockStatement<'ast>)
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct BlockStatement<'ast> {
    body: StatementList<'ast>,
}

pub type StatementNode<'ast> = Node<'ast, Statement<'ast>>;
pub type StatementList<'ast> = NodeList<'ast, Statement<'ast>>;

impl_from! {
    BlockStatement => Statement::BlockStatement,
}
