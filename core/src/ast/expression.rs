use ast::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Expression<'ast> {
    IdentifierExpression(Identifier<'ast>),
    PrimitiveExpression(Primitive<'ast>),
    PrefixExpression(PrefixExpression<'ast>),
    PostfixExpression(PostfixExpression<'ast>),
    BinaryExpression(BinaryExpression<'ast>),
    AssignmentExpression(AssignmentExpression<'ast>),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PrefixOperator {
    LogicalNot,
    BitNot,
    Delete,
    Increment,
    Decrement,
    Plus,
    Minus,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PostfixOperator {
    Increment,
    Decrement,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinaryOperator {
    Multiplication,
    Division,
    Remainder,
    Exponent,
    Addition,
    Subtraction,
    BitShiftLeft,
    BitShiftRight,
    Lesser,
    LesserEquals,
    Greater,
    GreaterEquals,
    Equality,
    Inequality,
    BitAnd,
    BitXor,
    BitOr,
    LogicalAnd,
    LogicalOr,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum AssignmentOperator {
    Plain,
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Remainder,
    BitShiftLeft,
    BitShiftRight,
    BitAnd,
    BitXor,
    BitOr,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PrefixExpression<'ast> {
    pub operator: Node<'ast, PrefixOperator>,
    pub operand: ExpressionNode<'ast>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PostfixExpression<'ast> {
    pub operand: ExpressionNode<'ast>,
    pub operator: Node<'ast, PostfixOperator>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct BinaryExpression<'ast> {
    pub left: ExpressionNode<'ast>,
    pub operator: Node<'ast, BinaryOperator>,
    pub right: ExpressionNode<'ast>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct AssignmentExpression<'ast> {
    pub left: ExpressionNode<'ast>,
    pub operator: Node<'ast, AssignmentOperator>,
    pub right: ExpressionNode<'ast>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Primitive<'ast> {
    BooleanLiteral(bool),
    IntegerLiteral(&'ast str),
    RationalLiteral(&'ast str),
    StringLiteral(&'ast str),
}

pub type ExpressionNode<'ast> = Node<'ast, Expression<'ast>>;
pub type ExpressionList<'ast> = NodeList<'ast, Expression<'ast>>;

impl_from! {
    Identifier => Expression::IdentifierExpression,
    Primitive => Expression::PrimitiveExpression,
    PrefixExpression => Expression::PrefixExpression,
    PostfixExpression => Expression::PostfixExpression,
    BinaryExpression => Expression::BinaryExpression,
    AssignmentExpression => Expression::AssignmentExpression,
}
