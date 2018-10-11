use {*};

/// A `SourceUnit` is the top level construct of the grammar.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SourceUnit<'ast> {
    PragmaDirective(PragmaDirective<'ast>),
    ImportDirective(ImportDirective<'ast>),
    ContractDefinition(ContractDefinition<'ast>),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PragmaDirective<'ast> {
    pub version: &'ast str,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Import<'ast> {
    pub symbol: IdentifierNode<'ast>,
    pub alias: Option<IdentifierNode<'ast>>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ImportDirective<'ast> {
    /// 'import' StringLiteral ('as' Identifier)? ';'
    Global {
        source: Node<'ast, StringLiteral<'ast>>,
        alias: Option<IdentifierNode<'ast>>,
    },

    /// 'import' ('*' | Identifier) ('as' Identifier)? 'from' StringLiteral ';'
    From {
        symbol: Option<IdentifierNode<'ast>>,
        alias: Option<IdentifierNode<'ast>>,
        source: Node<'ast, StringLiteral<'ast>>,
    },

    /// 'import' '{' Identifier ('as' Identifier)? ( ',' Identifier ('as' Identifier)? )* '}' 'from' StringLiteral ';'
    ManyFrom {
        imports: NodeList<'ast, Import<'ast>>,
        source: Node<'ast, StringLiteral<'ast>>,
    },
}

impl_from! {
    PragmaDirective => SourceUnit::PragmaDirective,
    ImportDirective => SourceUnit::ImportDirective,
    ContractDefinition => SourceUnit::ContractDefinition,
}
