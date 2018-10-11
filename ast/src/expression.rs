use {*};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Expression<'ast> {
    ThisExpression,
    IdentifierExpression(Identifier<'ast>),
    PrimitiveExpression(Primitive<'ast>),
    PrefixExpression(PrefixExpression<'ast>),
    PostfixExpression(PostfixExpression<'ast>),
    BinaryExpression(BinaryExpression<'ast>),
    AssignmentExpression(AssignmentExpression<'ast>),
    TupleExpression(TupleExpression<'ast>),
    CallExpression(CallExpression<'ast>),
    MemberAccessExpression(MemberAccessExpression<'ast>),
    IndexAccessExpression(IndexAccessExpression<'ast>),
    ConditionalExpression(ConditionalExpression<'ast>),
    ElementaryTypeExpression(ElementaryTypeName),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Primitive<'ast> {
    Bool(bool),
    HexNumber(&'ast str),
    IntegerNumber(&'ast str, NumberUnit),
    RationalNumber(&'ast str),
    String(&'ast str),
}

// TODO: Exact units
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum NumberUnit {
    None,
    Ether(EtherUnit),
    Time(TimeUnit),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TimeUnit {
    Years,
    Months,
    Weeks,
    Days,
    Hours,
    Minutes,
    Seconds,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum EtherUnit {
    Ether,
    Finney,
    Szabo,
    Wei,
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
pub struct TupleExpression<'ast> {
    pub expressions: ExpressionList<'ast>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct CallExpression<'ast> {
    pub callee: ExpressionNode<'ast>,
    pub arguments: ExpressionList<'ast>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct MemberAccessExpression<'ast> {
    pub object: ExpressionNode<'ast>,
    pub member: IdentifierNode<'ast>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct IndexAccessExpression<'ast> {
    pub array: ExpressionNode<'ast>,
    pub index: Option<ExpressionNode<'ast>>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ConditionalExpression<'ast> {
    pub test: ExpressionNode<'ast>,
    pub consequent: ExpressionNode<'ast>,
    pub alternate: ExpressionNode<'ast>,
}

pub use self::Expression::ThisExpression;

pub type ExpressionNode<'ast> = Node<'ast, Expression<'ast>>;
pub type ExpressionList<'ast> = NodeList<'ast, Expression<'ast>>;

impl<'ast> From<ElementaryTypeName> for Expression<'ast> {
    #[inline]
    fn from(val: ElementaryTypeName) -> Expression<'ast> {
        Expression::ElementaryTypeExpression(val)
    }
}

impl_from! {
    Identifier => Expression::IdentifierExpression,
    Primitive => Expression::PrimitiveExpression,
    PrefixExpression => Expression::PrefixExpression,
    PostfixExpression => Expression::PostfixExpression,
    BinaryExpression => Expression::BinaryExpression,
    AssignmentExpression => Expression::AssignmentExpression,
    TupleExpression => Expression::TupleExpression,
    CallExpression => Expression::CallExpression,
    MemberAccessExpression => Expression::MemberAccessExpression,
    IndexAccessExpression => Expression::IndexAccessExpression,
    ConditionalExpression => Expression::ConditionalExpression,
}
