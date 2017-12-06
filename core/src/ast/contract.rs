use ast::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ContractDefinition<'ast> {
    pub name: IdentifierNode<'ast>,
    pub inherits: IdentifierList<'ast>,
    pub body: NodeList<'ast, ContractPart<'ast>>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ContractPart<'ast> {
    StateVariableDeclaration,
    UsingForDeclaration,
    StructDefinition,
    ModifierDefinition,
    FunctionDefinition,
    EventDefinition(EventDefinition<'ast>),
    EnumDefinition,
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

impl_from! {
    EventDefinition => ContractPart::EventDefinition,
}
