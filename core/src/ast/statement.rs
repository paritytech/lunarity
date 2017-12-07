use ast::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Statement<'ast> {
    PlaceholderStatement,
    VariableDefinitionStatement(VariableDefinitionStatement<'ast>),
    TupleDeconstructionStatement(TupleDeconstructionStatement<'ast>),
    ExpressionStatement(ExpressionNode<'ast>),
    BlockStatement(BlockStatement<'ast>),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SimpleStatement<'ast> {
    VariableDefinitionStatement(VariableDefinitionStatement<'ast>),
    TupleDeconstructionStatement(TupleDeconstructionStatement<'ast>),
    ExpressionStatement(ExpressionNode<'ast>),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct BlockStatement<'ast> {
    pub body: StatementList<'ast>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct VariableDefinitionStatement<'ast> {
    pub declaration: VariableDeclarationNode<'ast>,
    pub init: Option<ExpressionNode<'ast>>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TupleDeconstructionStatement<'ast> {
    pub ids: IdentifierList<'ast>,
    pub init: ExpressionNode<'ast>,
}

pub type StatementNode<'ast> = Node<'ast, Statement<'ast>>;
pub type StatementList<'ast> = NodeList<'ast, Statement<'ast>>;
pub type BlockStatementNode<'ast> = Node<'ast, BlockStatement<'ast>>;

impl_from! {
    VariableDefinitionStatement => Statement::VariableDefinitionStatement,
    VariableDefinitionStatement => SimpleStatement::VariableDefinitionStatement,
    TupleDeconstructionStatement => Statement::TupleDeconstructionStatement,
    TupleDeconstructionStatement => SimpleStatement::TupleDeconstructionStatement,
    ExpressionNode => Statement::ExpressionStatement,
    ExpressionNode => SimpleStatement::ExpressionStatement,
    BlockStatement => Statement::BlockStatement,
}
