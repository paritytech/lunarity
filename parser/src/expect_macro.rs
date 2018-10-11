#[macro_export]
macro_rules! expect {
    ($parser:ident, $option:expr) => {
        match $option {
            Some(node) => node,
            None => {
                $parser.error();

                return None;
            }
        }
    }
}
