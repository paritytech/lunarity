use std::fmt::{self, Debug};
use std::ops::Range;
use Token;

/// Error type used by the tokenizer and the parser internally.
#[derive(PartialEq, Clone)]
pub struct Error {
    pub token: Token,
    pub raw: Box<str>,
    pub span: Range<usize>,
}

impl Debug for Error {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Unexpected {:?}({}) at {}:{}", &self.token, &*self.raw, self.span.start, self.span.end)
    }
}
