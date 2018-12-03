extern crate logos;

mod token;

pub use self::token::Token;
pub use logos::{Logos, lookup};
pub type Lexer<S> = logos::Lexer<Token, S>;

// FIXME: This should probably be handled with a callback
#[inline]
pub fn read_pragma<'source, S: logos::Source<'source>>(lex: &mut Lexer<S>) -> S::Slice {
    use logos::internal::LexerInternal;

    loop {
        match lex.read() {
            0x01...0x20 => lex.bump(),
            _           => break,
        }
    }

    let start = lex.range().start;

    loop {
        match lex.read() {
            0 => {
                lex.token = Token::UnexpectedEndOfProgram;
                let end = lex.range().end;

                return lex.source.slice(start..end).expect("0 guarantees being at the end; qed");
            },
            b';' => {
                let end = lex.range().end;

                let version = lex.source.slice(start..end).expect("Still within bounds; qed");

                lex.token = Token::Semicolon;
                lex.bump();

                return version;
            },
            _ => lex.bump(),
        }
    }
}


#[cfg(test)]
mod test {
    use super::*;
    use self::Token::*;
    use logos::Logos;

    fn assert_lex<T>(source: &str, tokens: T)
    where
        T: AsRef<[(Token, &'static str)]>
    {
        let mut lex = Token::lexer(source);

        for &(ref token, slice) in tokens.as_ref() {
            assert!(
                lex.token == *token && lex.slice() == slice,
                "\n\n\n\tExpected {:?}({:?}), found {:?}({:?}) instead!\n\n\n", token, slice, lex.token, lex.slice()
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
        assert_lex(" // foo\nbar", [(Identifier, "bar")]);
    }

    #[test]
    fn block_comment() {
        assert_lex(" /* foo */ bar", [(Identifier, "bar")]);
        assert_lex(" /* foo **/ bar", [(Identifier, "bar")]);
        assert_lex(" /* foo ***/ bar", [(Identifier, "bar")]);
        assert_lex(" /* foo ****/ bar", [(Identifier, "bar")]);
        assert_lex(" /* foo *****/ bar", [(Identifier, "bar")]);
        assert_lex(" /* foo ", [(UnexpectedEndOfProgram, "/* foo ")]);
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
    fn strings() {
        assert_lex(r#"
            foo
            "\x19Ethereum Signed Message:\n47Please take my Ether and try to build Polkadot."
        "#,
        &[
            (Identifier, "foo"),
            (LiteralString, r#""\x19Ethereum Signed Message:\n47Please take my Ether and try to build Polkadot.""#),
        ])
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
                (TypeBytes, "bytes"),
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
        let source = include_str!("../../lunarity/benches/second-price-auction.sol");

        let mut lex = Token::lexer(source);
        let mut tokens = 0;

        while lex.token != EndOfProgram {
            assert_ne!(lex.token, UnexpectedToken, "Unexpected: {} at {:?}", lex.slice(), lex.range());
            assert_ne!(lex.token, UnexpectedEndOfProgram);

            tokens += 1;

            lex.advance();
        }

        assert_eq!(tokens, 1299);
    }
}
