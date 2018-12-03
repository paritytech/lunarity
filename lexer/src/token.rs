//!
//!  Lookup table layout
//!  ===================
//!
//!  ```text
//!  EOF    ;      :      ,      .      (      )      {      }      [      ]      =>
//!  IDENT  BLTIN  CONTR  LIB    IFACE  ENUM   STRUCT MODIF  EVENT  FUNCT  VAR    ANON
//!  AS     ASM    BREAK  CONST  CONTIN DO     DELETE ELSE   EXTERN FOR    HEX    IF
//!  INDEX  INTERN IMPORT IS     MAP    MEM    NEW    PAY    PULIC  PRAGMA PRIV   PURE
//!  RET    RETNS  STORAG SUPER  THIS   THROW  USING  VIEW   WHILE  RESERV T_BOOL T_ADDR
//!  T_STR  T_BYT  T_BYTS T_INT  T_UINT T_FIX  T_UFIX L_TRUE L_FALS L_HEX  L_INT  L_RAT
//!  L_STR  E_ETH  E_FINN E_SZAB E_WEI  T_YEAR T_WEEK T_DAYS T_HOUR T_MIN  T_SEC  :=
//!  =:     ++     --     !      ~      *      /      %      **     +      -      <<
//!  >>     <      <=     >      >=     ==     !=     &      ^      |      &&     ||
//!  ?      =      +=     -=     *=     /=     %=     <<=    >>=    &=     ^=     |=
//!  ERRTOK ERREOF
//!  ```
//!

use logos::{Logos, Lexer, Extras, Source, Slice};

/// If the current token is an elementary type,
/// this will hold it's size, if applicable.
///
/// The first number is size in bytes, the second is
/// decimal offset for fixed point numbers.
///
/// - For `int64` this will be set to `(8, _)`
/// - For `bytes20` this will be set to `(20, _)`
/// - For 'ufixed128x40` this will be set to `(16, 40)`
#[derive(Default, Clone, Copy)]
pub struct TypeSize(pub u8, pub u8);

impl Extras for TypeSize {}

#[derive(Debug, PartialEq, Clone, Copy, Logos)]
#[extras = "TypeSize"]
pub enum Token {
    #[end]
    EndOfProgram,

    #[token = ";"]
    Semicolon,

    #[token = ":"]
    Colon,

    #[token = ","]
    Comma,

    #[token = "."]
    Accessor,

    #[token = "("]
    ParenOpen,

    #[token = ")"]
    ParenClose,

    #[token = "{"]
    BraceOpen,

    #[token = "}"]
    BraceClose,

    #[token = "["]
    BracketOpen,

    #[token = "]"]
    BracketClose,

    #[token = "=>"]
    Arrow,

    #[regex = "[a-zA-Z_$][a-zA-Z0-9_$]*"]
    Identifier,

    #[regex = "block|msg|tx|now|suicide|selfdestruct|addmod"]
    #[regex = "mulmod|sha3|keccak256|log0|log1|log2|log3|log4"]
    #[regex = "sha256|ecrecover|ripemd160|assert|revert|require"]
    IdentifierBuiltin,

    #[token = "contract"]
    DeclarationContract,

    #[token = "library"]
    DeclarationLibrary,

    #[token = "interface"]
    DeclarationInterface,

    #[token = "enum"]
    DeclarationEnum,

    #[token = "struct"]
    DeclarationStruct,

    #[token = "modifier"]
    DeclarationModifier,

    #[token = "event"]
    DeclarationEvent,

    #[token = "function"]
    DeclarationFunction,

    #[token = "var"]
    DeclarationVar,

    #[token = "anonymous"]
    KeywordAnonymous,

    #[token = "as"]
    KeywordAs,

    #[token = "assembly"]
    KeywordAssembly,

    #[token = "break"]
    KeywordBreak,

    #[token = "constant"]
    KeywordConstant,

    #[token = "continue"]
    KeywordContinue,

    #[token = "do"]
    KeywordDo,

    #[token = "delete"]
    KeywordDelete,

    #[token = "else"]
    KeywordElse,

    #[token = "external"]
    KeywordExternal,

    #[token = "for"]
    KeywordFor,

    // FIXME: Should able to handle hex literals on lexer-level!
    #[token = "hex"]
    KeywordHex,

    #[token = "if"]
    KeywordIf,

    #[token = "indexed"]
    KeywordIndexed,

    #[token = "internal"]
    KeywordInternal,

    #[token = "import"]
    KeywordImport,

    #[token = "is"]
    KeywordIs,

    #[token = "mapping"]
    KeywordMapping,

    #[token = "memory"]
    KeywordMemory,

    #[token = "new"]
    KeywordNew,

    #[token = "payable"]
    KeywordPayable,

    #[token = "public"]
    KeywordPublic,

    #[token = "pragma"]
    KeywordPragma,

    #[token = "private"]
    KeywordPrivate,

    #[token = "pure"]
    KeywordPure,

    #[token = "return"]
    KeywordReturn,

    #[token = "returns"]
    KeywordReturns,

    #[token = "storage"]
    KeywordStorage,

    #[token = "super"]
    KeywordSuper,

    #[token = "this"]
    KeywordThis,

    #[token = "throw"]
    KeywordThrow,

    #[token = "using"]
    KeywordUsing,

    #[token = "view"]
    KeywordView,

    #[token = "while"]
    KeywordWhile,

    #[regex = "abstract|after|case|catch|default|final|in"]
    #[regex = "inline|let|match|null|of|relocatable|static"]
    #[regex = "switch|try|type|typeof"]
    ReservedWord,

    #[token = "bool"]
    TypeBool,

    #[token = "address"]
    TypeAddress,

    #[token = "string"]
    TypeString,

    #[regex = "byte|bytes[1-2][0-9]?|bytes3[0-2]?|bytes[4-9]"]
    #[callback = "validate_bytes"]
    TypeByte,

    #[token = "bytes"]
    TypeBytes,

    #[token = "int"]
    #[callback = "default_size"]
    TypeInt,

    #[token = "uint"]
    #[callback = "default_size"]
    TypeUint,

    #[regex = "int(8|16|24|32|40|48|56|64|72|80|88|96|104|112|120|128|136|144)"]
    #[regex = "int(152|160|168|176|184|192|200|208|216|224|232|240|248|256)"]
    #[callback = "validate_int"]
    TypeIntN,

    #[regex = "uint(8|16|24|32|40|48|56|64|72|80|88|96|104|112|120|128|136|144)"]
    #[regex = "uint(152|160|168|176|184|192|200|208|216|224|232|240|248|256)"]
    #[callback = "validate_uint"]
    TypeUintN,

    #[regex = "fixed([1-9][0-9]?[0-9]?x[0-9][0-9]?)?"]
    #[callback = "validate_fixed"]
    TypeFixed,

    #[regex = "ufixed([1-9][0-9]?[0-9]?x[0-9][0-9]?)?"]
    #[callback = "validate_fixed"]
    TypeUfixed,

    #[token = "true"]
    LiteralTrue,

    #[token = "false"]
    LiteralFalse,

    #[regex = "0[xX][0-9a-fA-F]+"]
    LiteralHex,

    #[regex = "[0-9]+"]
    LiteralInteger,

    #[regex = "[0-9]*\\.[0-9]+([eE][+-]?[0-9]+)?|[0-9]+[eE][+-]?[0-9]+"]
    #[callback = "rational_to_integer"]
    LiteralRational,

    #[regex = "\"([^\"\\\\]|\\\\.)*\""]
    #[regex = "'([^'\\\\]|\\\\.)*'"]
    LiteralString,

    #[token = "ether"]
    UnitEther,

    #[token = "finney"]
    UnitFinney,

    #[token = "szabo"]
    UnitSzabo,

    #[token = "wei"]
    UnitWei,

    #[token = "years"]
    UnitTimeYears,

    #[token = "weeks"]
    UnitTimeWeeks,

    #[token = "days"]
    UnitTimeDays,

    #[token = "hours"]
    UnitTimeHours,

    #[token = "minutes"]
    UnitTimeMinutes,

    #[token = "seconds"]
    UnitTimeSeconds,

    #[token = ":="]
    AssemblyBind,

    #[token = "=:"]
    AssemblyAssign,

    #[token = "++"]
    OperatorIncrement,

    #[token = "--"]
    OperatorDecrement,

    #[token = "!"]
    OperatorLogicalNot,

    #[token = "~"]
    OperatorBitNot,

    #[token = "*"]
    OperatorMultiplication,

    #[token = "/"]
    OperatorDivision,

    #[token = "%"]
    OperatorRemainder,

    #[token = "**"]
    OperatorExponent,

    #[token = "+"]
    OperatorAddition,

    #[token = "-"]
    OperatorSubtraction,

    #[token = "<<"]
    OperatorBitShiftLeft,

    #[token = ">>"]
    OperatorBitShiftRight,

    #[token = "<"]
    OperatorLesser,

    #[token = "<="]
    OperatorLesserEquals,

    #[token = ">"]
    OperatorGreater,

    #[token = ">="]
    OperatorGreaterEquals,

    #[token = "=="]
    OperatorEquality,

    #[token = "!="]
    OperatorInequality,

    #[token = "&"]
    OperatorBitAnd,

    #[token = "^"]
    OperatorBitXor,

    #[token = "|"]
    OperatorBitOr,

    #[token = "&&"]
    OperatorLogicalAnd,

    #[token = "||"]
    OperatorLogicalOr,

    #[token = "?"]
    OperatorConditional,

    #[token = "="]
    Assign,

    #[token = "+="]
    AssignAddition,

    #[token = "-="]
    AssignSubtraction,

    #[token = "*="]
    AssignMultiplication,

    #[token = "/="]
    AssignDivision,

    #[token = "%="]
    AssignRemainder,

    #[token = "<<="]
    AssignBitShiftLeft,

    #[token = ">>="]
    AssignBitShiftRight,

    #[token = "&="]
    AssignBitAnd,

    #[token = "^="]
    AssignBitXor,

    #[token = "|="]
    AssignBitOr,

    #[regex = "//[^\n]*"]
    #[token = "/*"]
    #[callback = "ignore_comments"]
    #[error]
    UnexpectedToken,
    UnexpectedEndOfProgram,
}

fn ignore_comments<'source, Src: Source<'source>>(lex: &mut Lexer<Token, Src>) {
    use logos::internal::LexerInternal;

    if lex.slice().as_bytes() == b"/*" {
        loop {
            match lex.read() {
                0    => return lex.token = Token::UnexpectedEndOfProgram,
                b'*' => {
                    if lex.next() == b'/' {
                        lex.bump();
                        break;
                    }
                },
                _ => lex.bump(),
            }
        }
    }

    lex.advance();
}

fn validate_bytes<'source, Src: Source<'source>>(lex: &mut Lexer<Token, Src>) {
    let slice = lex.slice().as_bytes();

    if slice.len() > 5 {
        lex.extras.0 = slice[5] - b'0';

        if let Some(byte) = slice.get(6) {
            lex.extras.0 = lex.extras.0 * 10 + (byte - b'0');
        }
    } else {
        lex.extras.0 = 1;
    }
}

fn default_size<'source, Src: Source<'source>>(lex: &mut Lexer<Token, Src>) {
    lex.extras.0 = 32;
}

fn validate_int<'source, Src: Source<'source>>(lex: &mut Lexer<Token, Src>) {
    let slice = lex.slice().as_bytes();

    let mut n = (slice[3] - b'0') as u16;

    for byte in &slice[4..] {
        n = n * 10 + (*byte - b'0') as u16;
    }

    lex.extras.0 = (n / 8) as u8;
    lex.token = Token::TypeInt;
}

fn validate_uint<'source, Src: Source<'source>>(lex: &mut Lexer<Token, Src>) {
    let slice = lex.slice().as_bytes();

    let mut n = (slice[4] - b'0') as u16;

    for byte in &slice[5..] {
        n = n * 10 + (*byte - b'0') as u16;
    }

    lex.extras.0 = (n / 8) as u8;
    lex.token = Token::TypeUint;
}

fn validate_fixed<'source, Src: Source<'source>>(lex: &mut Lexer<Token, Src>) {
    let slice = lex.slice().as_bytes();
    let cutoff = if slice.starts_with(b"u") { 6 } else { 5 };

    let mut n = 0u16;
    let mut m = 0u8;

    let mut iter = slice[cutoff..].iter();

    while let Some(&byte) = iter.next() {
        if byte == b'x' {
            break;
        }

        n = n * 10 + (byte - b'0') as u16;
    }

    for byte in iter {
        m = m * 10 + (*byte - b'0');
    }

    if n % 8 != 0 || n > 256 || m > 80 {
        lex.token = Token::Identifier;
    } else {
        lex.extras.0 = (n / 8) as u8;
        lex.extras.1 = m;
    }
}

fn rational_to_integer<'source, Src: Source<'source>>(lex: &mut Lexer<Token, Src>) {
    let mut floating = 0i32;
    let mut iter = lex.slice().as_bytes().iter();

    'outer: while let Some(&byte) = iter.next() {
        match byte {
            b'e' | b'E' => break 'outer,
            b'0' => floating += 1,
            b'.' => {
                floating = 0;
                let mut zeroes = 0;

                while let Some(&byte) = iter.next() {
                    match byte {
                        b'e' | b'E' => break 'outer,
                        b'0' => zeroes += 1,
                        _ => {
                            floating -= 1 + zeroes;
                            zeroes = 0;
                        },
                    }
                }
            }
            _ => {},
        }
    }

    let mut neg = 1i32;
    let mut e = 0i32;

    for &byte in iter {
        match byte {
            b'-' => neg = -1,
            b'+' => {},
            byte => e = e * 10 + (byte - b'0') as i32,
        }
    }

    if floating + e * neg >= 0 {
        lex.token = Token::LiteralInteger;
    }
}
