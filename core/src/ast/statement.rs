use toolshed::list::List;

use ast::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Statement<'ast> {
    Placeholder,
    VariableDefinitionStatement(VariableDefinitionStatement<'ast>),
    InferredDefinitionStatement(InferredDefinitionStatement<'ast>),
    ExpressionStatement(ExpressionNode<'ast>),
    BlockStatement(Block<'ast>),
}

/// Used in the `for` loop initialization.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SimpleStatement<'ast> {
    VariableDefinitionStatement(VariableDefinitionStatement<'ast>),
    InferredDefinitionStatement(InferredDefinitionStatement<'ast>),
    ExpressionStatement(ExpressionNode<'ast>),
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
pub type BlockNode<'ast> = Node<'ast, Block<'ast>>;

impl_from! {
    VariableDefinitionStatement => Statement::VariableDefinitionStatement,
    VariableDefinitionStatement => SimpleStatement::VariableDefinitionStatement,
    InferredDefinitionStatement => Statement::InferredDefinitionStatement,
    InferredDefinitionStatement => SimpleStatement::InferredDefinitionStatement,
    ExpressionNode => Statement::ExpressionStatement,
    ExpressionNode => SimpleStatement::ExpressionStatement,
    Block => Statement::BlockStatement,
}
