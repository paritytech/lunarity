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
    TypeBytes,
    TypeInt,
    TypeUint,
    TypeFixed,
    TypeUfixed,
    LiteralTrue,
    LiteralFalse,
    LiteralHex,
    LiteralInteger,
    LiteralRational,
    LiteralString,
    UnitEther,
    UnitFinney,
    UnitSzabo,
    UnitWei,
    UnitTimeYears,
    UnitTimeWeeks,
    UnitTimeDays,
    UnitTimeHours,
    UnitTimeMinutes,
    UnitTimeSeconds,
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
