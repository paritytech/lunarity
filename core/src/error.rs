use std::fmt::{self, Debug};
use lexer::Token;

/// Error type used by the tokenizer and the parser internally.
#[derive(PartialEq, Clone)]
pub struct Error {
    pub token: Token,
    pub raw: Box<str>,
    pub start: usize,
    pub end: usize,
}

impl Debug for Error {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Unexpected {:?}({}) at {}:{}", &self.token, &*self.raw, self.start, self.end)
    }
}
