use ast::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ContractDefinition<'ast> {
    pub name: IdentifierNode<'ast>,
    pub inherits: IdentifierList<'ast>,
    pub body: ContractPartList<'ast>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ContractPart<'ast> {
    StateVariableDeclaration(StateVariableDeclaration<'ast>),
    UsingForDeclaration,
    StructDefinition,
    ModifierDefinition,
    FunctionDefinition(FunctionDefinition<'ast>),
    EventDefinition(EventDefinition<'ast>),
    EnumDefinition,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum StateVariableVisibility {
    Public,
    Internal,
    Private,
    Constant,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct StateVariableDeclaration<'ast> {
    pub type_name: TypeNameNode<'ast>,
    pub visibility: Option<StateVariableVisibility>,
    pub name: IdentifierNode<'ast>,
    pub init: Option<ExpressionNode<'ast>>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct EventDefinition<'ast> {
    pub anonymous: Option<FlagNode<'ast>>,
    pub name: IdentifierNode<'ast>,
    pub params: IndexedParameterList<'ast>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct IndexedParameter<'ast> {
    pub type_name: TypeNameNode<'ast>,
    pub indexed: Option<FlagNode<'ast>>,
    pub name: Option<IdentifierNode<'ast>>,
}

pub type IndexedParameterList<'ast> = NodeList<'ast, IndexedParameter<'ast>>;
pub type ContractPartNode<'ast> = Node<'ast, ContractPart<'ast>>;
pub type ContractPartList<'ast> = NodeList<'ast, ContractPart<'ast>>;

impl_from! {
    StateVariableDeclaration => ContractPart::StateVariableDeclaration,
    FunctionDefinition => ContractPart::FunctionDefinition,
    EventDefinition => ContractPart::EventDefinition,
}
