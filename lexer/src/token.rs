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

use logos::Logos;

#[derive(Debug, PartialEq, Clone, Copy, Logos)]
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

    // FIXME: Should able to handle hex literals!
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

    // FIXME: needs a validator
    #[regex = "byte|bytes[1-9][0-9]?"]
    TypeByte,

    #[token = "bytes"]
    TypeBytes,

    // FIXME: needs a validator
    #[regex = "int([1-9][0-9]?[0-9]?)?"]
    TypeInt,

    // FIXME: needs a validator
    #[regex = "uint([1-9][0-9]?[0-9]?)?"]
    TypeUint,

    // FIXME: needs a validator
    #[regex = "fixed([1-9][0-9]?[0-9]?x[0-9][0-9]?)?"]
    TypeFixed,

    // FIXME: needs a validator
    #[regex = "ufixed([1-9][0-9]?[0-9]?x[0-9][0-9]?)?"]
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
    LiteralRational,

    #[regex = r#"'([^']|\\')*'"#]
    #[regex = r#""([^"]|\\")*"|'([^']|\\')*'"#]
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

    #[error]
    UnexpectedToken,
    UnexpectedEndOfProgram,
}

