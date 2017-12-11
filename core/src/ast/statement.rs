use toolshed::list::List;

use ast::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Statement<'ast> {
    /// Only available in modifiers
    Placeholder,
    IfStatement(IfStatement<'ast>),
    WhileStatement(WhileStatement<'ast>),
    ForStatement(ForStatement<'ast>),
    BlockStatement(Block<'ast>),
    InlineAssemblyStatement(InlineAssemblyStatement<'ast>),
    DoWhileStatement(DoWhileStatement<'ast>),
    ContinueStatement,
    BreakStatement,
    ReturnStatement(ReturnStatement<'ast>),
    ThrowStatement,
    VariableDefinitionStatement(VariableDefinitionStatement<'ast>),
    InferredDefinitionStatement(InferredDefinitionStatement<'ast>),
    ExpressionStatement(ExpressionNode<'ast>),
}

/// Used in the `for` loop initialization.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SimpleStatement<'ast> {
    VariableDefinitionStatement(VariableDefinitionStatement<'ast>),
    InferredDefinitionStatement(InferredDefinitionStatement<'ast>),
    ExpressionStatement(ExpressionNode<'ast>),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct IfStatement<'ast> {
    pub test: ExpressionNode<'ast>,
    pub consequent: StatementNode<'ast>,
    pub alternate: Option<StatementNode<'ast>>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct WhileStatement<'ast> {
    pub test: ExpressionNode<'ast>,
    pub body: StatementNode<'ast>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ForStatement<'ast> {
    pub init: Option<SimpleStatementNode<'ast>>,
    pub test: Option<ExpressionNode<'ast>>,
    pub update: Option<ExpressionNode<'ast>>,
    pub body: StatementNode<'ast>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Block<'ast> {
    pub body: StatementList<'ast>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct InlineAssemblyStatement<'ast> {
    pub string: Option<StringLiteralNode<'ast>>,
    pub block: InlineAssemblyBlockNode<'ast>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct DoWhileStatement<'ast> {
    pub body: StatementNode<'ast>,
    pub test: ExpressionNode<'ast>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ReturnStatement<'ast> {
    pub value: Option<ExpressionNode<'ast>>,
}

/// explicitly typed, can have storage flag, init is optional
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct VariableDefinitionStatement<'ast> {
    pub declaration: VariableDeclarationNode<'ast>,
    pub init: Option<ExpressionNode<'ast>>,
}

/// type inferred via `var`, cannot have storage flag, init is mandatory
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct InferredDefinitionStatement<'ast> {
    pub ids: List<'ast, Option<IdentifierNode<'ast>>>,
    pub init: ExpressionNode<'ast>,
}

pub use self::Statement::{Placeholder, BreakStatement, ContinueStatement, ThrowStatement};

pub type StatementNode<'ast> = Node<'ast, Statement<'ast>>;
pub type StatementList<'ast> = NodeList<'ast, Statement<'ast>>;
pub type SimpleStatementNode<'ast> = Node<'ast, SimpleStatement<'ast>>;
pub type BlockNode<'ast> = Node<'ast, Block<'ast>>;

impl_from! {
    IfStatement => Statement::IfStatement,
    WhileStatement => Statement::WhileStatement,
    ForStatement => Statement::ForStatement,
    DoWhileStatement => Statement::DoWhileStatement,
    ReturnStatement => Statement::ReturnStatement,
    VariableDefinitionStatement => Statement::VariableDefinitionStatement,
    VariableDefinitionStatement => SimpleStatement::VariableDefinitionStatement,
    InferredDefinitionStatement => Statement::InferredDefinitionStatement,
    InferredDefinitionStatement => SimpleStatement::InferredDefinitionStatement,
    ExpressionNode => Statement::ExpressionStatement,
    ExpressionNode => SimpleStatement::ExpressionStatement,
    Block => Statement::BlockStatement,
    InlineAssemblyStatement => Statement::InlineAssemblyStatement,
}
