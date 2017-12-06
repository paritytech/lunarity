use ast::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Expression;

pub type ExpressionNode<'ast> = Node<'ast, Expression>;
pub type ExpressionList<'ast> = NodeList<'ast, Expression>;
