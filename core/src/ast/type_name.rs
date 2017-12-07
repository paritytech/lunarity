use ast::*;

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum TypeName<'ast> {
    ElementaryTypeName(ElementaryTypeName),
    UserDefinedTypeName(Identifier<'ast>),
    Mapping,
    ArrayTypeName,
    FunctionTypeName,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct VariableDeclaration<'ast> {
    pub type_name: TypeNameNode<'ast>,
    pub location: Option<Node<'ast, StorageLocation>>,
    pub id: IdentifierNode<'ast>,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum StorageLocation {
    Memory,
    Storage,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum ElementaryTypeName {
    Address,
    Bool,
    String,
    Var,
    Int(u8),
    Uint(u8),
    Byte(u8),
    Fixed(u8, u8),
    Ufixed(u8, u8),
}

pub type TypeNameNode<'ast> = Node<'ast, TypeName<'ast>>;
pub type VariableDeclarationNode<'ast> = Node<'ast, VariableDeclaration<'ast>>;
pub type VariableDeclarationList<'ast> = NodeList<'ast, VariableDeclaration<'ast>>;

impl<'ast> From<ElementaryTypeName> for TypeName<'ast> {
    #[inline]
    fn from(elementary: ElementaryTypeName) -> Self {
        TypeName::ElementaryTypeName(elementary)
    }
}

impl_from! {
    Identifier => TypeName::UserDefinedTypeName,
}
