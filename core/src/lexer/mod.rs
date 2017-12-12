mod token;
mod labels;
mod util;

pub use lexer::token::Token;

use lexer::util::legal_in_label;
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
    L_P, IDT, L_R, L_S, L_T, L_U, L_V, L_W, IDT, L_Y, IDT, BEO, PIP, BEC, TLD, ERR, // 7
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
    lex.token = match lex.next_byte() {
        b'=' => {
            lex.bump();

            AssemblyBind
        },

        _ => Colon
    };
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
                        return lex.consume();
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
                    0 => return lex.token = UnexpectedEndOfProgram,
                    _ => {
                        if lex.read_pair() == *b"*/" {
                            lex.index += 2;
                            return lex.consume();
                        }
                    }
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
        b'0'...b'9' => {
            lex.bump();

            return lex.token = UnexpectedToken;
        },
        b'.' => {
            lex.bump();

            return lex.read_float(0);
        },
        b'e' | b'E' => {
            lex.bump();

            return lex.read_scientific(0);
        }
        _ => {},
    }

    lex.token = LiteralInteger;
});

// 1 to 9
const DIG: ByteHandler = Some(|lex| {
    let mut floating = 0;

    unwind_loop!({
        match lex.next_byte() {
            b'0'        => floating += 1,
            b'1'...b'9' => floating = 0,
            b'.' => {
                lex.bump();

                return lex.read_float(0);
            },
            b'e' | b'E' => {
                lex.bump();

                return lex.read_scientific(floating);
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

            return lex.read_float(0);
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

    /// If the current token is an elementary type,
    /// this will hold it's size, if applicable.
    ///
    /// The first number is size in bytes, the second is
    /// decimal offset for fixed point numbers.
    ///
    /// - For `int64` this will be set to `(8, _)`
    /// - For `bytes20` this will be set to `(20, _)`
    /// - For 'ufixed128x40` this will be set to `(16, 40)`
    pub type_size: (u8, u8),

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
            type_size: (0, 0),
            ptr,
            index: 0,
            token_start: 0,
            _phantom: PhantomData,
        };

        lexer.consume();

        lexer
    }

    /// Advances the lexer, produces a new `Token` and stores it on `self.token`.
    #[inline]
    pub fn consume(&mut self) {
        let mut ch;

        loop {
            ch = self.read_byte();

            if let Some(handler) = self.handler_from_byte(ch) {
                self.token_start = self.index;
                return handler(self);
            }

            self.bump();
        }
    }

    /// Create an `&str` slice from source spanning current token.
    #[inline]
    pub fn token_as_str(&self) -> &'arena str {
        let start = self.token_start;
        self.slice_from(start)
    }

    #[inline]
    fn handler_from_byte(&mut self, byte: u8) -> ByteHandler {
        BYTE_HANDLERS[byte as usize]
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

    /// Get the start position of the current token, then consume the lexer.
    #[inline]
    pub fn start_then_consume(&mut self) -> u32 {
        let start = self.start();
        self.consume();
        start
    }

    /// Get the end position of the current token, then consume the lexer.
    #[inline]
    pub fn end_then_advance(&mut self) -> u32 {
        let end = self.end();
        self.consume();
        end
    }

    pub fn invalid_token(&mut self) -> Error {
        let start = self.token_start;
        let end = self.index;
        let token = self.token;

        if token != EndOfProgram {
            self.consume();
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

    /// This is safe if `read_byte` does not return `0`
    #[inline]
    fn read_pair(&self) -> [u8; 2] {
        unsafe { *(self.ptr.offset(self.index as isize) as *const [u8; 2]) }
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
    fn read_label(&mut self) {
        while legal_in_label(self.read_byte()) {
            self.bump();
        }
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
    pub fn read_pragma(&mut self) -> &'arena str {
        loop {
            match self.read_byte() {
                0x01...0x20 => self.bump(),
                _           => break,
            }
        }

        let start = self.index;

        loop {
            match self.read_byte() {
                0    => {
                    self.token = UnexpectedEndOfProgram;

                    return self.slice_from(start);
                },
                b';' => {
                    let version = self.slice_from(start);

                    self.token_start = self.index;
                    self.token = Semicolon;
                    self.bump();

                    return version;
                },
                _    => self.bump(),
            }
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

        self.token = LiteralHex;
    }

    #[inline]
    fn read_float(&mut self, mut floating: i32) {
        let mut zeroes = 0;

        loop {
            match self.read_byte() {
                b'0' => {
                    self.bump();

                    zeroes += 1;
                },
                b'1'...b'9'  => {
                    self.bump();

                    floating -= 1 + zeroes;
                    zeroes = 0;
                },
                b'e' | b'E'  => {
                    self.bump();
                    return self.read_scientific(floating);
                },
                _            => break
            }
        }

        self.token = if floating < 0 {
            LiteralRational
        } else {
            LiteralInteger
        }
    }

    #[inline]
    fn read_scientific(&mut self, floating: i32) {
        let neg = match self.read_byte() {
            b'-' => { self.bump(); true },
            b'+' => { self.bump(); false },
            _    => false,
        };

        let mut e = match self.read_byte() {
            byte @ b'0'...b'9' => {
                self.bump();

                (byte - b'0') as i32
            },
            _ => {
                self.bump();

                return self.token = UnexpectedToken;
            },
        };

        for _ in 0..5 {
            match self.read_byte() {
                byte @ b'0'...b'9' => {
                    e = e * 10 + (byte - b'0') as i32;
                    self.bump();
                },
                _ => break
            }
        }

        if neg { e *= -1; }

        self.token = if floating + e < 0 {
            LiteralRational
        } else {
            LiteralInteger
        };
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
            lex.consume();
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
    fn identifiers() {
        assert_lex(
            "
                foo _foo $foo $_foo _ $ $$ fooBar BarFoo foo10 $1
            ",
             &[
                (Identifier, "foo"),
                (Identifier, "_foo"),
                (Identifier, "$foo"),
                (Identifier, "$_foo"),
                (Identifier, "_"),
                (Identifier, "$"),
                (Identifier, "$$"),
                (Identifier, "fooBar"),
                (Identifier, "BarFoo"),
                (Identifier, "foo10"),
                (Identifier, "$1"),
            ][..]
        );
    }

    #[test]
    fn controls() {
        assert_lex(
            "
                ; : , . ( ) { } [ ] =>
            ",
             &[
                (Semicolon, ";"),
                (Colon, ":"),
                (Comma, ","),
                (Accessor, "."),
                (ParenOpen, "("),
                (ParenClose, ")"),
                (BraceOpen, "{"),
                (BraceClose, "}"),
                (BracketOpen, "["),
                (BracketClose, "]"),
                (Arrow, "=>"),
            ][..]
        );
    }

    #[test]
    fn literals() {
        assert_lex(
            r#"
                true false 0 42 0xDEAD 0Xdead 3.14 3.14E+2 .12345
                5.1e2 42e-3 500E-1 500.1 10.000 'foo bar' "doge to the moon"
            "#,
             &[
                (LiteralTrue, "true"),
                (LiteralFalse, "false"),
                (LiteralInteger, "0"),
                (LiteralInteger, "42"),
                (LiteralHex, "0xDEAD"),
                (LiteralHex, "0Xdead"),
                (LiteralRational, "3.14"),
                (LiteralInteger, "3.14E+2"),
                (LiteralRational, ".12345"),
                (LiteralInteger, "5.1e2"),
                (LiteralRational, "42e-3"),
                (LiteralInteger, "500E-1"),
                (LiteralRational, "500.1"),
                (LiteralInteger, "10.000"),
                (LiteralString, "'foo bar'"),
                (LiteralString, "\"doge to the moon\""),
            ][..]
        );
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
    fn units() {
        assert_lex(
            "
                wei szabo finney ether
                seconds minutes hours days weeks years
            ",
             &[
                (UnitWei, "wei"),
                (UnitSzabo, "szabo"),
                (UnitFinney, "finney"),
                (UnitEther, "ether"),
                (UnitTimeSeconds, "seconds"),
                (UnitTimeMinutes, "minutes"),
                (UnitTimeHours, "hours"),
                (UnitTimeDays, "days"),
                (UnitTimeWeeks, "weeks"),
                (UnitTimeYears, "years"),
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
                (ReservedWord, "abstract"),
                (ReservedWord, "after"),
                (ReservedWord, "case"),
                (ReservedWord, "catch"),
                (ReservedWord, "default"),
                (ReservedWord, "final"),
                (ReservedWord, "in"),
                (ReservedWord, "inline"),
                (ReservedWord, "let"),
                (ReservedWord, "match"),
                (ReservedWord, "null"),
                (ReservedWord, "of"),
                (ReservedWord, "relocatable"),
                (ReservedWord, "static"),
                (ReservedWord, "switch"),
                (ReservedWord, "try"),
                (ReservedWord, "type"),
                (ReservedWord, "typeof"),
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

    #[test]
    fn types_easy() {
        assert_lex(
            "
                bool int uint string byte bytes address fixed ufixed
            ",
             &[
                (TypeBool, "bool"),
                (TypeInt, "int"),
                (TypeUint, "uint"),
                (TypeString, "string"),
                (TypeByte, "byte"),
                (TypeByte, "bytes"),
                (TypeAddress, "address"),
                (TypeFixed, "fixed"),
                (TypeUfixed, "ufixed"),
            ][..]
        );
    }

    #[test]
    fn types_bytes() {
        assert_lex(
            "
                bytes1  bytes2  bytes3  bytes4  bytes5  bytes6  bytes7  bytes8
                bytes9  bytes10 bytes11 bytes12 bytes13 bytes14 bytes15 bytes16
                bytes17 bytes18 bytes19 bytes20 bytes21 bytes22 bytes23 bytes24
                bytes25 bytes26 bytes27 bytes28 bytes29 bytes30 bytes31 bytes32
            ",
             &[
                (TypeByte, "bytes1"),
                (TypeByte, "bytes2"),
                (TypeByte, "bytes3"),
                (TypeByte, "bytes4"),
                (TypeByte, "bytes5"),
                (TypeByte, "bytes6"),
                (TypeByte, "bytes7"),
                (TypeByte, "bytes8"),
                (TypeByte, "bytes9"),
                (TypeByte, "bytes10"),
                (TypeByte, "bytes11"),
                (TypeByte, "bytes12"),
                (TypeByte, "bytes13"),
                (TypeByte, "bytes14"),
                (TypeByte, "bytes15"),
                (TypeByte, "bytes16"),
                (TypeByte, "bytes17"),
                (TypeByte, "bytes18"),
                (TypeByte, "bytes19"),
                (TypeByte, "bytes20"),
                (TypeByte, "bytes21"),
                (TypeByte, "bytes22"),
                (TypeByte, "bytes23"),
                (TypeByte, "bytes24"),
                (TypeByte, "bytes25"),
                (TypeByte, "bytes26"),
                (TypeByte, "bytes27"),
                (TypeByte, "bytes28"),
                (TypeByte, "bytes29"),
                (TypeByte, "bytes30"),
                (TypeByte, "bytes31"),
                (TypeByte, "bytes32"),
            ][..]
        );
    }

    #[test]
    fn types_int() {
        assert_lex(
            "
                int8   int16  int24  int32  int40  int48  int56  int64
                int72  int80  int88  int96  int104 int112 int120 int128
                int136 int144 int152 int160 int168 int176 int184 int192
                int200 int208 int216 int224 int232 int240 int248 int256
            ",
             &[
                (TypeInt, "int8"),
                (TypeInt, "int16"),
                (TypeInt, "int24"),
                (TypeInt, "int32"),
                (TypeInt, "int40"),
                (TypeInt, "int48"),
                (TypeInt, "int56"),
                (TypeInt, "int64"),
                (TypeInt, "int72"),
                (TypeInt, "int80"),
                (TypeInt, "int88"),
                (TypeInt, "int96"),
                (TypeInt, "int104"),
                (TypeInt, "int112"),
                (TypeInt, "int120"),
                (TypeInt, "int128"),
                (TypeInt, "int136"),
                (TypeInt, "int144"),
                (TypeInt, "int152"),
                (TypeInt, "int160"),
                (TypeInt, "int168"),
                (TypeInt, "int176"),
                (TypeInt, "int184"),
                (TypeInt, "int192"),
                (TypeInt, "int200"),
                (TypeInt, "int208"),
                (TypeInt, "int216"),
                (TypeInt, "int224"),
                (TypeInt, "int232"),
                (TypeInt, "int240"),
                (TypeInt, "int248"),
                (TypeInt, "int256"),
            ][..]
        );
    }

    #[test]
    fn types_uint() {
        assert_lex(
            "
                uint8   uint16  uint24  uint32  uint40  uint48  uint56  uint64
                uint72  uint80  uint88  uint96  uint104 uint112 uint120 uint128
                uint136 uint144 uint152 uint160 uint168 uint176 uint184 uint192
                uint200 uint208 uint216 uint224 uint232 uint240 uint248 uint256
            ",
             &[
                (TypeUint, "uint8"),
                (TypeUint, "uint16"),
                (TypeUint, "uint24"),
                (TypeUint, "uint32"),
                (TypeUint, "uint40"),
                (TypeUint, "uint48"),
                (TypeUint, "uint56"),
                (TypeUint, "uint64"),
                (TypeUint, "uint72"),
                (TypeUint, "uint80"),
                (TypeUint, "uint88"),
                (TypeUint, "uint96"),
                (TypeUint, "uint104"),
                (TypeUint, "uint112"),
                (TypeUint, "uint120"),
                (TypeUint, "uint128"),
                (TypeUint, "uint136"),
                (TypeUint, "uint144"),
                (TypeUint, "uint152"),
                (TypeUint, "uint160"),
                (TypeUint, "uint168"),
                (TypeUint, "uint176"),
                (TypeUint, "uint184"),
                (TypeUint, "uint192"),
                (TypeUint, "uint200"),
                (TypeUint, "uint208"),
                (TypeUint, "uint216"),
                (TypeUint, "uint224"),
                (TypeUint, "uint232"),
                (TypeUint, "uint240"),
                (TypeUint, "uint248"),
                (TypeUint, "uint256"),
            ][..]
        );
    }

    #[test]
    fn types_fixed_ufixed() {
        assert_lex(
            "
                fixed8x0  fixed8x1  fixed16x2  fixed256x80  fixed144x57
                ufixed8x0 ufixed8x1 ufixed16x2 ufixed256x80 ufixed144x57
            ",
             &[
                (TypeFixed, "fixed8x0"),
                (TypeFixed, "fixed8x1"),
                (TypeFixed, "fixed16x2"),
                (TypeFixed, "fixed256x80"),
                (TypeFixed, "fixed144x57"),
                (TypeUfixed, "ufixed8x0"),
                (TypeUfixed, "ufixed8x1"),
                (TypeUfixed, "ufixed16x2"),
                (TypeUfixed, "ufixed256x80"),
                (TypeUfixed, "ufixed144x57"),
            ][..]
        );
    }

    #[test]
    fn not_real_types() {
        assert_lex(
            "
                bytes33 int127 fixed127 fixed128x fixed258x80 fixed256x81
                bytes0  uint0  uint53   ufixed1x1
            ",
             &[
                (Identifier, "bytes33"),
                (Identifier, "int127"),
                (Identifier, "fixed127"),
                (Identifier, "fixed128x"),
                (Identifier, "fixed258x80"),
                (Identifier, "fixed256x81"),
                (Identifier, "bytes0"),
                (Identifier, "uint0"),
                (Identifier, "uint53"),
                (Identifier, "ufixed1x1"),
            ][..]
        );
    }

    #[test]
    fn second_price_auction() {
        let source = include_str!("../../benches/second-price-auction.sol");

        let arena = Arena::new();
        let mut lex = Lexer::new(&arena, source);

        let mut tokens = 0;

        while lex.token != EndOfProgram {
            assert_ne!(lex.token, UnexpectedToken);
            assert_ne!(lex.token, UnexpectedEndOfProgram);

            tokens += 1;

            lex.consume();
        }

        assert_eq!(tokens, 1299);
    }
}
