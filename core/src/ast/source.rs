/// A `SourceUnit` is the top level construct of the grammar.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SourceUnit<'ast> {
    PragmaDirective(PragmaDirective<'ast>),
    ImportDirective,
    ContractDefinition,
    LibraryDefinition,
    InterfaceDefinition,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PragmaDirective<'ast> {
    pub version: &'ast str,
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
}
