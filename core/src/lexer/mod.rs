mod token;
mod labels;
mod util;

pub use lexer::token::*;

use lexer::labels::*;
use lexer::token::Token::*;

use std::str;
use std::marker::PhantomData;
use error::Error;
use toolshed::Arena;

macro_rules! expect_byte {
    ($lex:ident) => ({
        match $lex.read_byte() {
            0 => return $lex.token = UnexpectedEndOfProgram,
            _ => $lex.bump()
        }
    });
}

macro_rules! unwind_loop {
    ($iteration:expr) => ({
        $iteration
        $iteration
        $iteration
        $iteration
        $iteration

        loop {
            $iteration
            $iteration
            $iteration
            $iteration
            $iteration
        }
    })
}

type ByteHandler = Option<for<'arena> fn(&mut Lexer<'arena>)>;

/// Lookup table mapping any incoming byte to a handler function defined below.
static BYTE_HANDLERS: [ByteHandler; 256] = [
//   0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F   //
    EOF, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, // 0
    ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, // 1
    ___, EXL, QOT, ERR, IDT, PRC, AMP, QOT, PNO, PNC, ATR, PLS, COM, MIN, PRD, SLH, // 2
    ZER, DIG, DIG, DIG, DIG, DIG, DIG, DIG, DIG, DIG, COL, SEM, LSS, EQL, MOR, QST, // 3
    ERR, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, // 4
    IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, IDT, BTO, IDT, BTC, CRT, IDT, // 5
    ERR, L_A, L_B, L_C, L_D, L_E, L_F, IDT, L_H, L_I, IDT, L_K, L_L, L_M, L_N, L_O, // 6
    L_P, IDT, L_R, L_S, L_T, L_U, L_V, L_W, IDT, IDT, IDT, BEO, PIP, BEC, TLD, ERR, // 7
    ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, // 8
    ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, // 9
    ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, // A
    ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, // B
    ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, // C
    ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, // D
    ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, // E
    ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, // F
];

const ___: ByteHandler = None;

const ERR: ByteHandler = Some(|lex| {
    lex.bump();

    lex.token = UnexpectedToken;
});

const EOF: ByteHandler = Some(|lex| {
    lex.token = EndOfProgram;
});

// ;
const SEM: ByteHandler = Some(|lex| {
    lex.bump();

    lex.token = Semicolon;
});

// :
const COL: ByteHandler = Some(|lex| {
    lex.bump();

    lex.token = Colon;
});

// ,
const COM: ByteHandler = Some(|lex| {
    lex.bump();

    lex.token = Comma;
});

// (
const PNO: ByteHandler = Some(|lex| {
    lex.bump();

    lex.token = ParenOpen;
});

// )
const PNC: ByteHandler = Some(|lex| {
    lex.bump();

    lex.token = ParenClose;
});

// [
const BTO: ByteHandler = Some(|lex| {
    lex.bump();

    lex.token = BracketOpen;
});

// ]
const BTC: ByteHandler = Some(|lex| {
    lex.bump();

    lex.token = BracketClose;
});

// {
const BEO: ByteHandler = Some(|lex| {
    lex.bump();

    lex.token = BraceOpen;
});

// }
const BEC: ByteHandler = Some(|lex| {
    lex.bump();

    lex.token = BraceClose;
});

// =
const EQL: ByteHandler = Some(|lex| {
    lex.token = match lex.next_byte() {
        b'=' => {
            lex.bump();

            OperatorEquality
        },

        b'>' => {
            lex.bump();

            Arrow
        },

        _ => Assign
    };
});

// !
const EXL: ByteHandler = Some(|lex| {
    lex.token = match lex.next_byte() {
        b'=' => {
            lex.bump();

            OperatorInequality
        },

        _ => OperatorLogicalNot
    };
});

// <
const LSS: ByteHandler = Some(|lex| {
    lex.token = match lex.next_byte() {
        b'<' => {
            match lex.next_byte() {
                b'=' => {
                    lex.bump();

                    AssignBitShiftLeft
                },

                _ => OperatorBitShiftLeft
            }
        },

        b'=' => {
            lex.bump();

            OperatorLesserEquals
        },

        _ => OperatorLesser
    };
});

// >
const MOR: ByteHandler = Some(|lex| {
    lex.token = match lex.next_byte() {
        b'>' => {
            match lex.next_byte() {
                b'=' => {
                    lex.bump();

                    AssignBitShiftRight
                },

                _ => OperatorBitShiftRight
            }
        },

        b'=' => {
            lex.bump();

            OperatorGreaterEquals
        },

        _ => OperatorGreater
    };
});

// ?
const QST: ByteHandler = Some(|lex| {
    lex.bump();

    lex.token = OperatorConditional;
});

// ~
const TLD: ByteHandler = Some(|lex| {
    lex.bump();

    lex.token = OperatorBitNot;
});

// ^
const CRT: ByteHandler = Some(|lex| {
    lex.token = match lex.next_byte() {
        b'=' => {
            lex.bump();

            AssignBitXor
        },

        _ => OperatorBitXor
    };
});

// &
const AMP: ByteHandler = Some(|lex| {
    lex.token = match lex.next_byte() {
        b'&' => {
            lex.bump();

            OperatorLogicalAnd
        },

        b'=' => {
            lex.bump();

            AssignBitAnd
        },

        _ => OperatorBitAnd
    };
});

// |
const PIP: ByteHandler = Some(|lex| {
    lex.token = match lex.next_byte() {
        b'|' => {
            lex.bump();

            OperatorLogicalOr
        },

        b'=' => {
            lex.bump();

            AssignBitOr
        },

        _ => OperatorBitOr
    };
});

// +
const PLS: ByteHandler = Some(|lex| {
    lex.token = match lex.next_byte() {
        b'+' => {
            lex.bump();

            OperatorIncrement
        },

        b'=' => {
            lex.bump();

            AssignAddition
        },

        _ => OperatorAddition
    };
});

// -
const MIN: ByteHandler = Some(|lex| {
    lex.token = match lex.next_byte() {
        b'-' => {
            lex.bump();

            OperatorDecrement
        },

        b'=' => {
            lex.bump();

            AssignSubtraction
        },

        _ => OperatorSubtraction
    };
});

// *
const ATR: ByteHandler = Some(|lex| {
    lex.token = match lex.next_byte() {
        b'*' => {
            lex.bump();

            OperatorExponent
        },

        b'=' => {
            lex.bump();

            AssignMultiplication
        },

        _ => OperatorMultiplication
    };
});

// /
const SLH: ByteHandler = Some(|lex| {
    lex.token = match lex.next_byte() {
        // regular comment
        b'/' => {
            // Keep consuming bytes until new line or end of source
            unwind_loop!({
                match lex.next_byte() {
                    0 | b'\n' => {
                        return lex.advance();
                    }
                    _ => {}
                }
            });
        },

        // block comment
        b'*' => {
            // Keep consuming bytes until */ happens in a row
            unwind_loop!({
                match lex.next_byte() {
                    b'*' => {
                        match lex.next_byte() {
                            b'/' => {
                                lex.bump();
                                return lex.advance();
                            },
                            0 => return lex.token = UnexpectedEndOfProgram,
                            _ => {}
                        }
                    },
                    0 => return lex.token = UnexpectedEndOfProgram,
                    _ => {}
                }
            });
        },

        b'=' => {
            lex.bump();

            AssignDivision
        }

        _ => OperatorDivision
    };
});

// %
const PRC: ByteHandler = Some(|lex| {
    lex.token = match lex.next_byte() {
        b'=' => {
            lex.bump();

            AssignRemainder
        },

        _ => OperatorRemainder
    };
});

// 0
const ZER: ByteHandler = Some(|lex| {
    match lex.next_byte() {
        b'x' | b'X' => {
            lex.bump();

            return lex.read_hexadec();
        },

        _ => {}
    }

    loop {
        match lex.read_byte() {
            b'0'...b'9' => {
                lex.bump();
            },
            b'.' => {
                lex.bump();

                return lex.read_float();
            },
            b'e' | b'E' => {
                lex.bump();

                return lex.read_scientific();
            }
            _ => break,
        }
    }

    lex.token = LiteralInteger;
});

// 1 to 9
const DIG: ByteHandler = Some(|lex| {
    unwind_loop!({
        match lex.next_byte() {
            b'0'...b'9' => {},
            b'.' => {
                lex.bump();

                return lex.read_float();
            },
            b'e' | b'E' => {
                lex.bump();

                return lex.read_scientific();
            },
            _ => {
                return lex.token = LiteralInteger;
            },
        }
    });
});

// .
const PRD: ByteHandler = Some(|lex| {
    lex.token = match lex.next_byte() {
        b'0'...b'9' => {
            lex.bump();

            return lex.read_float();
        },

        _ => Accessor,
    };
});

// " or '
const QOT: ByteHandler = Some(|lex| {
    let style = lex.read_byte();

    lex.bump();

    unwind_loop!({
        match lex.read_byte() {
            ch if ch == style => {
                lex.bump();
                return lex.token = LiteralString;
            },
            b'\\' => {
                lex.bump();
                expect_byte!(lex);
            },
            0 => {
                return lex.token = UnexpectedEndOfProgram;
            },
            _ => lex.bump()
        }
    });
});

pub struct Lexer<'arena> {
    /// Current `Token` from the source.
    pub token: Token,

    /// Source to parse, must be a C-style buffer ending with 0 byte
    ptr: *const u8,

    /// Current index
    index: usize,

    /// Position of current token in source
    token_start: usize,

    _phantom: PhantomData<&'arena str>,
}


impl<'arena> Lexer<'arena> {
    /// Create a new `Lexer` from source using an existing arena.
    #[inline]
    pub fn new(arena: &'arena Arena, source: &str) -> Self {
        unsafe { Lexer::from_ptr(arena.alloc_str_with_nul(source)) }
    }

    /// Create a new `Lexer` from a raw pointer to byte string.
    ///
    /// **The source must be null terminated!**
    /// Passing a pointer that is not null terminated is undefined behavior!
    ///
    /// **The source must be valid UTF8!**
    /// Passing a pointer to data that is not valid UTF8 will lead
    /// to bugs or undefined behavior.
    #[inline]
    pub unsafe fn from_ptr(ptr: *const u8) -> Self {
        let mut lexer = Lexer {
            token: UnexpectedToken,
            ptr,
            index: 0,
            token_start: 0,
            _phantom: PhantomData,
        };

        lexer.advance();

        lexer
    }

    /// Advances the lexer, produces a new `Token` and stores it on `self.token`.
    #[inline]
    pub fn advance(&mut self) {
        let mut ch;

        unwind_loop!({
            ch = self.read_byte();

            if let Some(handler) = self.handler_from_byte(ch) {
                self.token_start = self.index;
                return handler(self);
            }

            self.bump();
        })
    }

    /// Create an `&str` slice from source spanning current token.
    #[inline]
    pub fn token_as_str(&self) -> &'arena str {
        let start = self.token_start;
        self.slice_from(start)
    }

    #[inline]
    fn handler_from_byte(&mut self, byte: u8) -> ByteHandler {
        unsafe { *(&BYTE_HANDLERS as *const ByteHandler).offset(byte as isize) }
    }

    /// Get the start and end positions of the current token.
    #[inline]
    pub fn loc(&self) -> (u32, u32) {
        (self.start(), self.end())
    }

    /// Get the start position of the current token.
    #[inline]
    pub fn start(&self) -> u32 {
        self.token_start as u32
    }

    /// Get the end position of the current token.
    #[inline]
    pub fn end(&self) -> u32 {
        self.index as u32
    }

    /// Get the start position of the current token, then advance the lexer.
    #[inline]
    pub fn start_then_advance(&mut self) -> u32 {
        let start = self.start();
        self.advance();
        start
    }

    /// Get the end position of the current token, then advance the lexer.
    #[inline]
    pub fn end_then_advance(&mut self) -> u32 {
        let end = self.end();
        self.advance();
        end
    }

    pub fn invalid_token(&mut self) -> Error {
        let start = self.token_start;
        let end = self.index;
        let token = self.token;

        if token != EndOfProgram {
            self.advance();
        }

        Error {
            token,
            start,
            end,
            raw: self.slice_source(start, end).to_owned().into_boxed_str()
        }
    }

    /// Read a byte from the source. Note that this does not increment
    /// the index. In few cases (all of them related to number parsing)
    /// we want to peek at the byte before doing anything. This will,
    /// very very rarely, lead to a situation where the same byte is read
    /// twice, but since this operation is using a raw pointer, the cost
    /// is virtually irrelevant.
    #[inline]
    fn read_byte(&self) -> u8 {
        unsafe { *self.ptr.offset(self.index as isize) }
    }

    /// Manually increment the index. Calling `read_byte` and then `bump`
    /// is equivalent to consuming a byte on an iterator.
    #[inline]
    fn bump(&mut self) {
        self.index += 1;
    }

    #[inline]
    fn next_byte(&mut self) -> u8 {
        self.bump();
        self.read_byte()
    }

    #[inline]
    fn read_label(&mut self) -> &'arena str {
        let start = self.token_start;

        unwind_loop!({
            if util::legal_in_label(self.read_byte()) {
                self.bump();
            } else {
                return self.slice_from(start)
            }
        })
    }

    #[inline]
    fn slice_from(&self, start: usize) -> &'arena str {
        let end = self.index;
        self.slice_source(start, end)
    }

    #[inline]
    fn slice_source(&self, start: usize, end: usize) -> &'arena str {
        use std::str::from_utf8_unchecked;
        use std::slice::from_raw_parts;

        unsafe {
            from_utf8_unchecked(from_raw_parts(
                self.ptr.offset(start as isize), end - start
            ))
        }
    }

    #[inline]
    fn read_hexadec(&mut self) {
        loop {
            match self.read_byte() {
                b'0'...b'9' |
                b'a'...b'f' |
                b'A'...b'F' => self.bump(),
                _           => break
            };
        }

        self.token = LiteralInteger;
    }

    #[inline]
    fn read_float(&mut self) {
        loop {
            match self.read_byte() {
                b'0'...b'9'  => self.bump(),
                b'e' | b'E'  => {
                    self.bump();
                    return self.read_scientific();
                },
                _            => break
            }
        }

        self.token = LiteralFloat;
    }

    #[inline]
    fn read_scientific(&mut self) {
        match self.read_byte() {
            b'-' | b'+' => self.bump(),
            _           => {}
        }

        loop {
            match self.read_byte() {
                b'0'...b'9' => self.bump(),
                _           => break
            }
        }

        self.token = LiteralFloat;
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn assert_lex<T>(source: &str, tokens: T)
    where
        T: AsRef<[(Token, &'static str)]>
    {
        let arena = Arena::new();
        let mut lex = Lexer::new(&arena, source);

        for &(ref token, slice) in tokens.as_ref() {
            assert!(
                lex.token == *token && lex.token_as_str() == slice,
                "\n\n\n\tExpected {:?}({:?}), found {:?}({:?}) instead!\n\n\n", token, slice, lex.token, lex.token_as_str()
            );
            lex.advance();
        }

        assert_eq!(lex.token, EndOfProgram);
    }

    #[test]
    fn empty_lexer() {
        assert_lex("   ", []);
    }

    #[test]
    fn line_comment() {
        assert_lex(" // foo", []);
    }

    #[test]
    fn block_comment() {
        assert_lex(" /* foo */ bar", [(Identifier, "bar")]);
    }

    #[test]
    fn keywords() {
        assert_lex(
            "
                anonymous as assembly break constant continue do
                delete else external for hex if indexed internal import
                is mapping memory new payable public pragma private pure
                return returns storage super this throw using view while
            ",
             &[
                (KeywordAnonymous, "anonymous"),
                (KeywordAs, "as"),
                (KeywordAssembly, "assembly"),
                (KeywordBreak, "break"),
                (KeywordConstant, "constant"),
                (KeywordContinue, "continue"),
                (KeywordDo, "do"),
                (KeywordDelete, "delete"),
                (KeywordElse, "else"),
                (KeywordExternal, "external"),
                (KeywordFor, "for"),
                (KeywordHex, "hex"),
                (KeywordIf, "if"),
                (KeywordIndexed, "indexed"),
                (KeywordInternal, "internal"),
                (KeywordImport, "import"),
                (KeywordIs, "is"),
                (KeywordMapping, "mapping"),
                (KeywordMemory, "memory"),
                (KeywordNew, "new"),
                (KeywordPayable, "payable"),
                (KeywordPublic, "public"),
                (KeywordPragma, "pragma"),
                (KeywordPrivate, "private"),
                (KeywordPure, "pure"),
                (KeywordReturn, "return"),
                (KeywordReturns, "returns"),
                (KeywordStorage, "storage"),
                (KeywordSuper, "super"),
                (KeywordThis, "this"),
                (KeywordThrow, "throw"),
                (KeywordUsing, "using"),
                (KeywordView, "view"),
                (KeywordWhile, "while"),
            ][..]
        );
    }

    #[test]
    fn declarations() {
        assert_lex(
            "
                var function event modifier struct
                enum contract library interface
            ",
             &[
                (DeclarationVar, "var"),
                (DeclarationFunction, "function"),
                (DeclarationEvent, "event"),
                (DeclarationModifier, "modifier"),
                (DeclarationStruct, "struct"),
                (DeclarationEnum, "enum"),
                (DeclarationContract, "contract"),
                (DeclarationLibrary, "library"),
                (DeclarationInterface, "interface"),
            ][..]
        );
    }

    #[test]
    fn reserved_words() {
        assert_lex(
            "
                abstract after case catch default final in
                inline let match null of relocatable static
                switch try type typeof
            ",
             &[
                (KeywordReserved, "abstract"),
                (KeywordReserved, "after"),
                (KeywordReserved, "case"),
                (KeywordReserved, "catch"),
                (KeywordReserved, "default"),
                (KeywordReserved, "final"),
                (KeywordReserved, "in"),
                (KeywordReserved, "inline"),
                (KeywordReserved, "let"),
                (KeywordReserved, "match"),
                (KeywordReserved, "null"),
                (KeywordReserved, "of"),
                (KeywordReserved, "relocatable"),
                (KeywordReserved, "static"),
                (KeywordReserved, "switch"),
                (KeywordReserved, "try"),
                (KeywordReserved, "type"),
                (KeywordReserved, "typeof"),
            ][..]
        );
    }

    #[test]
    fn builtins() {
        assert_lex(
            "
                block msg tx now suicide selfdestruct addmod
                mulmod sha3 keccak256 log0 log1 log2 log3 log4
                sha256 ecrecover ripemd160 assert revert require
            ",
             &[
                (IdentifierBuiltin, "block"),
                (IdentifierBuiltin, "msg"),
                (IdentifierBuiltin, "tx"),
                (IdentifierBuiltin, "now"),
                (IdentifierBuiltin, "suicide"),
                (IdentifierBuiltin, "selfdestruct"),
                (IdentifierBuiltin, "addmod"),
                (IdentifierBuiltin, "mulmod"),
                (IdentifierBuiltin, "sha3"),
                (IdentifierBuiltin, "keccak256"),
                (IdentifierBuiltin, "log0"),
                (IdentifierBuiltin, "log1"),
                (IdentifierBuiltin, "log2"),
                (IdentifierBuiltin, "log3"),
                (IdentifierBuiltin, "log4"),
                (IdentifierBuiltin, "sha256"),
                (IdentifierBuiltin, "ecrecover"),
                (IdentifierBuiltin, "ripemd160"),
                (IdentifierBuiltin, "assert"),
                (IdentifierBuiltin, "revert"),
                (IdentifierBuiltin, "require"),
            ][..]
        );
    }

    #[test]
    fn operators() {
        assert_lex(
            "
                ++ -- ! ~ * / % ** + - << >>
                < <= > >= == != & ^ | && || ?
                = += -= *= /= %= <<= >>= &= ^= |=
            ",
             &[
                (OperatorIncrement, "++"),
                (OperatorDecrement, "--"),
                (OperatorLogicalNot, "!"),
                (OperatorBitNot, "~"),
                (OperatorMultiplication, "*"),
                (OperatorDivision, "/"),
                (OperatorRemainder, "%"),
                (OperatorExponent, "**"),
                (OperatorAddition, "+"),
                (OperatorSubtraction, "-"),
                (OperatorBitShiftLeft, "<<"),
                (OperatorBitShiftRight, ">>"),
                (OperatorLesser, "<"),
                (OperatorLesserEquals, "<="),
                (OperatorGreater, ">"),
                (OperatorGreaterEquals, ">="),
                (OperatorEquality, "=="),
                (OperatorInequality, "!="),
                (OperatorBitAnd, "&"),
                (OperatorBitXor, "^"),
                (OperatorBitOr, "|"),
                (OperatorLogicalAnd, "&&"),
                (OperatorLogicalOr, "||"),
                (OperatorConditional, "?"),
                (Assign, "="),
                (AssignAddition, "+="),
                (AssignSubtraction, "-="),
                (AssignMultiplication, "*="),
                (AssignDivision, "/="),
                (AssignRemainder, "%="),
                (AssignBitShiftLeft, "<<="),
                (AssignBitShiftRight, ">>="),
                (AssignBitAnd, "&="),
                (AssignBitXor, "^="),
                (AssignBitOr, "|="),
            ][..]
        );
    }
}
