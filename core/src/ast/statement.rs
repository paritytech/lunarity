use toolshed::list::List;

use ast::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Statement<'ast> {
    /// Only available in modifiers
    Placeholder,
    IfStatement(IfStatement<'ast>),
    WhileStatement,
    ForStatement,
    BlockStatement(Block<'ast>),
    InlineAssemblyStatement,
    DoWhileStatement,
    ContinueStatement,
    BreakStatement,
    ReturnStatement,
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
pub struct Block<'ast> {
    pub body: StatementList<'ast>,
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

pub type StatementNode<'ast> = Node<'ast, Statement<'ast>>;
pub type StatementList<'ast> = NodeList<'ast, Statement<'ast>>;
pub type SimpleStatementNode<'ast> = Node<'ast, SimpleStatement<'ast>>;
pub type BlockNode<'ast> = Node<'ast, Block<'ast>>;

impl_from! {
    IfStatement => Statement::IfStatement,
    VariableDefinitionStatement => Statement::VariableDefinitionStatement,
    VariableDefinitionStatement => SimpleStatement::VariableDefinitionStatement,
    InferredDefinitionStatement => Statement::InferredDefinitionStatement,
    InferredDefinitionStatement => SimpleStatement::InferredDefinitionStatement,
    ExpressionNode => Statement::ExpressionStatement,
    ExpressionNode => SimpleStatement::ExpressionStatement,
    Block => Statement::BlockStatement,
}
