use ast::*;

/// A `SourceUnit` is the top level construct of the grammar.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SourceUnit<'ast> {
    PragmaDirective(PragmaDirective<'ast>),
    ImportDirective(ImportDirective<'ast>),
    ContractDefinition,
    LibraryDefinition,
    InterfaceDefinition,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PragmaDirective<'ast> {
    pub version: &'ast str,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Import<'ast> {
    pub symbol: Node<'ast, Identifier<'ast>>,
    pub alias: Option<Node<'ast, Identifier<'ast>>>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ImportDirective<'ast> {
    /// 'import' StringLiteral ('as' Identifier)? ';'
    Global {
        source: Node<'ast, StringLiteral<'ast>>,
        alias: Option<Node<'ast, Identifier<'ast>>>,
    },

    /// 'import' ('*' | Identifier) ('as' Identifier)? 'from' StringLiteral ';'
    From {
        symbol: Option<Node<'ast, Identifier<'ast>>>,
        alias: Option<Node<'ast, Identifier<'ast>>>,
        source: Node<'ast, StringLiteral<'ast>>,
    },

    /// 'import' '{' Identifier ('as' Identifier)? ( ',' Identifier ('as' Identifier)? )* '}' 'from' StringLiteral ';'
    ManyFrom {
        imports: NodeList<'ast, Import<'ast>>,
        source: Node<'ast, StringLiteral<'ast>>,
    },
}

macro_rules! impl_from {
    ($( $type:ident => $variant:ident, )*) => ($(
        impl<'ast> From<$type<'ast>> for SourceUnit<'ast> {
            #[inline]
            fn from(val: $type<'ast>) -> Self {
                SourceUnit::$variant(val)
            }
        }
    )*)
}

impl_from! {
    PragmaDirective => PragmaDirective,
    ImportDirective => ImportDirective,
}
