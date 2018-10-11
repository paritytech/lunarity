#[macro_export]
macro_rules! impl_from {
    ($( $type:ident => $enum:ident :: $variant:ident, )*) => ($(
        impl<'ast> From<$type<'ast>> for $enum<'ast> {
            #[inline]
            fn from(val: $type<'ast>) -> Self {
                $enum::$variant(val)
            }
        }
    )*)
}
