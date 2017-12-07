use ast::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct FunctionDefinition<'ast> {
    pub name: Option<IdentifierNode<'ast>>,
    pub params: ParameterList<'ast>,
    pub visibility: Option<FunctionVisibility>,
    pub state: Option<StateMutability>,
    pub returns: ParameterList<'ast>,
    pub body: Option<BlockStatement<'ast>>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FunctionVisibility {
    External,
    Public,
    Internal,
    Private,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum StateMutability {
    Pure,
    Constant,
    View,
    Payable,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Parameter<'ast> {
    pub type_name: TypeNameNode<'ast>,
    pub name: IdentifierNode<'ast>,
}

pub type ParameterList<'ast> = NodeList<'ast, Parameter<'ast>>;
