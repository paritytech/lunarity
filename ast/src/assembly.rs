use {*};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct InlineAssemblyBlock<'ast> {
    pub items: AssemblyItemList<'ast>
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum AssemblyItem<'ast> {
    Identifier(Identifier<'ast>),
    FunctionalAssemblyExpression(FunctionalAssemblyExpression<'ast>),
    InlineAssemblyBlock(InlineAssemblyBlock<'ast>),
    AssemblyLocalBinding(AssemblyLocalBinding<'ast>),
    AssemblyAssignment(AssemblyAssignment<'ast>),
    AssemblyLabel(AssemblyLabel<'ast>),
    NumberLiteral(Primitive<'ast>),

    // FIXME
    StringLiteral,

    // FIXME,
    HexLiteral,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct AssemblyLocalBinding<'ast> {
    pub id: IdentifierNode<'ast>,
    pub init: FunctionalAssemblyExpressionNode<'ast>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct AssemblyAssignment<'ast> {
    pub id: IdentifierNode<'ast>,
    pub init: FunctionalAssemblyExpressionNode<'ast>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct AssemblyLabel<'ast> {
    pub id: IdentifierNode<'ast>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct FunctionalAssemblyExpression<'ast> {
    pub id: IdentifierNode<'ast>,
    pub arguments: AssemblyItemList<'ast>,
}

pub type AssemblyItemNode<'ast> = Node<'ast, AssemblyItem<'ast>>;
pub type AssemblyItemList<'ast> = NodeList<'ast, AssemblyItem<'ast>>;
pub type FunctionalAssemblyExpressionNode<'ast> = Node<'ast, FunctionalAssemblyExpression<'ast>>;
pub type InlineAssemblyBlockNode<'ast> = Node<'ast, InlineAssemblyBlock<'ast>>;

impl_from! {
    Identifier => AssemblyItem::Identifier,
    FunctionalAssemblyExpression => AssemblyItem::FunctionalAssemblyExpression,
    InlineAssemblyBlock => AssemblyItem::InlineAssemblyBlock,
    AssemblyLocalBinding => AssemblyItem::AssemblyLocalBinding,
    AssemblyAssignment => AssemblyItem::AssemblyAssignment,
    AssemblyLabel => AssemblyItem::AssemblyLabel,
    Primitive => AssemblyItem::NumberLiteral,
}
