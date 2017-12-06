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
    FunctionDefinition,
    EventDefinition(EventDefinition<'ast>),
    EnumDefinition,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Visibility {
    Public,
    Internal,
    Private,
    Constant,
    Unspecified,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct StateVariableDeclaration<'ast> {
    pub type_name: TypeNameNode<'ast>,
    pub visibility: Visibility,
    pub name: IdentifierNode<'ast>,
    pub init: Option<ExpressionNode<'ast>>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct EventDefinition<'ast> {
    pub anonymous: bool,
    pub name: IdentifierNode<'ast>,
    pub params: IndexedParameterList<'ast>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct IndexedParameter<'ast> {
    pub indexed: bool,
    pub type_name: TypeNameNode<'ast>,
    pub name: IdentifierNode<'ast>,
}

pub type IndexedParameterList<'ast> = NodeList<'ast, IndexedParameter<'ast>>;
pub type ContractPartNode<'ast> = Node<'ast, ContractPart<'ast>>;
pub type ContractPartList<'ast> = NodeList<'ast, ContractPart<'ast>>;

impl_from! {
    StateVariableDeclaration => ContractPart::StateVariableDeclaration,
    EventDefinition => ContractPart::EventDefinition,
}
