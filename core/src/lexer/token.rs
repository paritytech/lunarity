#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Token {
    EndOfProgram,
    Semicolon,
    Colon,
    Comma,
    Accessor,
    ParenOpen,
    ParenClose,
    BraceOpen,
    BraceClose,
    BracketOpen,
    BracketClose,
    Arrow,
    Identifier,
    IdentifierBuiltin,
    DeclarationContract,
    DeclarationLibrary,
    DeclarationInterface,
    DeclarationEnum,
    DeclarationStruct,
    DeclarationModifier,
    DeclarationEvent,
    DeclarationFunction,
    DeclarationVar,
    KeywordAnonymous,
    KeywordAs,
    KeywordAssembly,
    KeywordBreak,
    KeywordConstant,
    KeywordContinue,
    KeywordDo,
    KeywordDelete,
    KeywordElse,
    KeywordExternal,
    KeywordFor,
    KeywordHex,
    KeywordIf,
    KeywordIndexed,
    KeywordInternal,
    KeywordImport,
    KeywordIs,
    KeywordMapping,
    KeywordMemory,
    KeywordNew,
    KeywordPayable,
    KeywordPublic,
    KeywordPragma,
    KeywordPrivate,
    KeywordPure,
    KeywordReturn,
    KeywordReturns,
    KeywordStorage,
    KeywordSuper,
    KeywordThis,
    KeywordThrow,
    KeywordUsing,
    KeywordView,
    KeywordWhile,
    ReservedWord,
    TypeBool,
    TypeAddress,
    TypeString,
    TypeByte,
    TypeInt,
    TypeUint,
    TypeFixed,
    TypeUfixed,
    LiteralTrue,
    LiteralFalse,
    LiteralHex,
    LiteralInteger,
    LiteralDecimal,
    LiteralString,
    UnitEther,                // wei | szabo | finney | ether
    UnitTime,                 // seconds | minutes | hours | days | weeks | years
    AssemblyBind,             // :=
    AssemblyAssign,           // =:
    OperatorIncrement,        //      ++ … | … ++
    OperatorDecrement,        //      -- … | … --
    OperatorLogicalNot,       //       ! …
    OperatorBitNot,           //       ~ …
    OperatorMultiplication,   //   …  *  …
    OperatorDivision,         //   …  /  …
    OperatorRemainder,        //   …  %  …
    OperatorExponent,         //   …  ** …
    OperatorAddition,         //   …  +  … | + …
    OperatorSubtraction,      //   …  -  … | - …
    OperatorBitShiftLeft,     //   …  << …
    OperatorBitShiftRight,    //   …  >> …
    OperatorLesser,           //   …  <  …
    OperatorLesserEquals,     //   …  <= …
    OperatorGreater,          //   …  >  …
    OperatorGreaterEquals,    //   …  >= …
    OperatorEquality,         //   …  == …
    OperatorInequality,       //   …  != …
    OperatorBitAnd,           //   …  &  …
    OperatorBitXor,           //   …  ^  …
    OperatorBitOr,            //   …  |  …
    OperatorLogicalAnd,       //   …  && …
    OperatorLogicalOr,        //   …  || …
    OperatorConditional,      //   …  ?  …  :  …
    Assign,                   //   …  =  …
    AssignAddition,           //   …  += …
    AssignSubtraction,        //   …  -= …
    AssignMultiplication,     //   …  *= …
    AssignDivision,           //   …  /= …
    AssignRemainder,          //   …  %= …
    AssignBitShiftLeft,       //   … <<= …
    AssignBitShiftRight,      //   … >>= …
    AssignBitAnd,             //   …  &= …
    AssignBitXor,             //   …  ^= …
    AssignBitOr,              //   …  |= …
    UnexpectedToken,
    UnexpectedEndOfProgram,
}
