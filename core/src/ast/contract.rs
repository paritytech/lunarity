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
    UsingForDeclaration(UsingForDeclaration<'ast>),
    StructDefinition(StructDefinition<'ast>),
    ModifierDefinition,
    FunctionDefinition(FunctionDefinition<'ast>),
    EventDefinition(EventDefinition<'ast>),
    EnumDefinition(EnumDefinition<'ast>),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct StateVariableDeclaration<'ast> {
    pub type_name: TypeNameNode<'ast>,
    pub visibility: Option<Node<'ast, StateVariableVisibility>>,
    pub name: IdentifierNode<'ast>,
    pub init: Option<ExpressionNode<'ast>>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum StateVariableVisibility {
    Public,
    Internal,
    Private,
    Constant,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct UsingForDeclaration<'ast> {
    pub id: IdentifierNode<'ast>,
    pub type_name: Option<TypeNameNode<'ast>>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct StructDefinition<'ast> {
    pub name: IdentifierNode<'ast>,
    pub body: VariableDeclarationList<'ast>,
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct EnumDefinition<'ast> {
    pub name: IdentifierNode<'ast>,
    pub variants: IdentifierList<'ast>,
}

pub type IndexedParameterList<'ast> = NodeList<'ast, IndexedParameter<'ast>>;
pub type ContractPartNode<'ast> = Node<'ast, ContractPart<'ast>>;
pub type ContractPartList<'ast> = NodeList<'ast, ContractPart<'ast>>;

impl_from! {
    StateVariableDeclaration => ContractPart::StateVariableDeclaration,
    UsingForDeclaration => ContractPart::UsingForDeclaration,
    StructDefinition => ContractPart::StructDefinition,
    FunctionDefinition => ContractPart::FunctionDefinition,
    EventDefinition => ContractPart::EventDefinition,
    EnumDefinition => ContractPart::EnumDefinition,
}
