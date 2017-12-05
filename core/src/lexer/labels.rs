use lexer::{util, ByteHandler};
use lexer::token::Token::*;

macro_rules! byte {
    // Mapping alphabet to ASCII bytes
    (a) => (b'a'); (b) => (b'b'); (c) => (b'c'); (d) => (b'd');
    (e) => (b'e'); (f) => (b'f'); (g) => (b'g'); (h) => (b'h');
    (i) => (b'i'); (j) => (b'j'); (k) => (b'k'); (l) => (b'l');
    (m) => (b'm'); (n) => (b'n'); (o) => (b'o'); (p) => (b'p');
    (q) => (b'q'); (r) => (b'r'); (s) => (b's'); (t) => (b't');
    (u) => (b'u'); (v) => (b'v'); (w) => (b'w'); (x) => (b'x');
    (y) => (b'y'); (z) => (b'z');

    // Mapping digits to ASCII bytes
    (_0) => (b'0'); (_1) => (b'1'); (_2) => (b'2'); (_3) => (b'3'); (_4) => (b'4');
    (_5) => (b'5'); (_6) => (b'6'); (_7) => (b'7'); (_8) => (b'8'); (_9) => (b'9');

    // Also allows patterns in brackets
    (($pat:pat)) => ($pat);
}

macro_rules! match_label {
    // Check if the label is followed by a 0..256 number divisible by 8
    ($lex:ident [ $( $byte:ident )* @ bits256 => $token:expr ]) => {
        unimplemented!();
    };

    // Check if the label is followed by a fixed number range
    ($lex:ident [ $( $byte:ident )* @ bits256x80 => $token:expr ]) => {
        unimplemented!();
    };

    // Parse a sequence of label bytes
    ($lex:ident [ $( $byte:ident )* => $token:expr ]) => {
        if $(
            $lex.next_byte() == byte!($byte) &&
        )* {$lex.bump(); !util::legal_in_label($lex.read_byte())} {
            return $lex.token = $token;
        }
    };

    // Parse a sequence followed by a switch
    ($lex:ident [
        $( $byte:ident )+
        $(
            [ $( $cont:tt )* ]
        )+
    ]) => {
        if $(
            $lex.next_byte() == byte!($byte) &&
        )* true {
            match_label!($lex [
                $(
                    [ $( $cont )* ]
                )*
            ]);
        }
    };

    // Switch over the first byte with empty variant
    ($lex:ident [
        [ => $token:expr ]
        $(
        [ $match:tt $( $cont:tt )* ]
        )+
    ]) => {
        match $lex.next_byte() {
            $(
                byte!($match) => match_label!($lex [ $( $cont )* ] ),
            )*
            ch if !util::legal_in_label(ch) => return $lex.token = $token,
            _ => {}
        }
    };

    // Switch over the first byte
    ($lex:ident [
        $( [ $match:tt $( $cont:tt )* ] )+
    ]) => {
        match $lex.next_byte() {
            $(
                byte!($match) => match_label!($lex [ $( $cont )* ] ),
            )*
            _ => {}
        }
    }
}

// Non-keyword Identifier: starting with a letter, _ or $
pub const IDT: ByteHandler = Some(|lex| {
    lex.bump();
    lex.read_label();
    lex.token = Identifier;
});

// Identifier or keyword starting with a letter `b`
pub const L_A: ByteHandler = Some(|lex| {
    match_label!(lex [
        [ b s t r a c t => KeywordReserved ]    // abstract
        [ d d
            [ r e s s   => KeywordType ]
            [ m o d     => IdentifierBuiltin ]
        ]
        [ f t e r       => KeywordReserved ]    // after
        [ n o n y m o u s => KeywordAnonymous ]
        [ s
            [             => KeywordAs ]
            [ s e
                [ m b l y => KeywordAssembly ]
                [ r t     => IdentifierBuiltin ]
            ]
        ]
    ]);

    lex.read_label();
    lex.token = Identifier;
});

// Identifier or keyword starting with a letter `b`
pub const L_B: ByteHandler = Some(|lex| {
    match_label!(lex [
        [ l o c k => IdentifierBuiltin ] // block
        [ r e a k => KeywordBreak ]
        [ o o l   => KeywordType ]
        [ y t e
            [     => KeywordType ]        // byte
            [ s
                [       => KeywordType ]  // bytes
                [ _1
                    [               => KeywordType ] // bytes1
                    [ (b'0'...b'9') => KeywordType ] // bytes10 - bytes19
                ]
                [ _2
                    [               => KeywordType ] // bytes2
                    [ (b'0'...b'9') => KeywordType ] // bytes20 - bytes29
                ]
                [ _3
                    [               => KeywordType ] // bytes3
                    [ (b'0'...b'2') => KeywordType ] // bytes30 - bytes32
                ]
                [ (b'4'...b'9')     => KeywordType ] // bytes4 - bytes9
            ]
        ]
    ]);

    lex.read_label();
    lex.token = Identifier;
});

// Identifier or keyword starting with a letter `c`
pub const L_C: ByteHandler = Some(|lex| {
    match_label!(lex [
        [ a
            [ s e    => KeywordReserved ] // case
            [ t c h  => KeywordReserved ] // catch
        ]
        [ o n
            [ s t a n t => KeywordConstant ]
            [ t
                [ i n u e => KeywordContinue ]
                [ r a c t => DeclarationContract ]
            ]
        ]
    ]);

    lex.read_label();
    lex.token = Identifier;
});

// Identifier or keyword starting with a letter `d`
pub const L_D: ByteHandler = Some(|lex| {
    match_label!(lex [
        [ o             => KeywordDo ]
        [ e
            [ l e t e   => KeywordDelete ]
            [ f a u l t => KeywordReserved ] // default
        ]
    ]);

    lex.read_label();
    lex.token = Identifier;
});

// Identifier or keyword starting with a letter `e`
pub const L_E: ByteHandler = Some(|lex| {
    match_label!(lex [
        [ c r e c o v e r => IdentifierBuiltin ] // ecrecover
        [ l s e           => KeywordElse ]
        [ n u m           => DeclarationEnum ]
        [ t h e r         => UnitEther ]         // ether
        [ v e n t         => DeclarationEvent ]
        [ x t e r n a l   => KeywordExternal ]
    ]);

    lex.read_label();
    lex.token = Identifier;
});

// Identifier or keyword starting with a letter `f`
pub const L_F: ByteHandler = Some(|lex| {
    match_label!(lex [
        [ i n a l       => KeywordReserved ] //final
        [ o r           => KeywordFor ]
        [ u n c t i o n => DeclarationFunction ]
        [ a l s e       => LiteralFalse ]
    ]);

    lex.read_label();
    lex.token = Identifier;
});


// Identifier or keyword starting with a letter `f`
pub const L_H: ByteHandler = Some(|lex| {
    match_label!(lex [
        [ e x     => KeywordHex ]
        [ o u r s => UnitTime ]   // hours
    ]);

    lex.read_label();
    lex.token = Identifier;
});

// Identifier or keyword starting with a letter `i`
pub const L_I: ByteHandler = Some(|lex| {
    match_label!(lex [
        [ n
            [               => KeywordReserved ] // in
            [ l i n e       => KeywordReserved ] // inline
            [ t
                [             => KeywordType ]   // int
                [ e r
                    [ f a c e => DeclarationInterface ]
                    [ n a l   => KeywordInternal ]
                ]
                [ (b'0'...b'9') @ bits256 => KeywordType ]
            ]
            [ d e x e d      => KeywordIndexed ]
        ]
        [ f         => KeywordIf ]
        [ s         => KeywordIs ]
        [ m p o r t => KeywordImport ]
    ]);

    lex.read_label();
    lex.token = Identifier;
});


// Identifier or keyword starting with a letter `i`
pub const L_K: ByteHandler = Some(|lex| {
    match_label!(lex [ e c c a k _2 _5 _6 => IdentifierBuiltin ]); // keccak256

    lex.read_label();
    lex.token = Identifier;
});

// Identifier or keyword starting with a letter `l`
pub const L_L: ByteHandler = Some(|lex| {
    match_label!(lex [
        [ e t         => KeywordReserved ] // let
        [ i b r a r y => DeclarationLibrary ]
        [ o g
            [ (b'0'...b'4') => IdentifierBuiltin ]
        ]
    ]);

    lex.read_label();
    lex.token = Identifier;
});

// Identifier or keyword starting with a letter `n`
pub const L_M: ByteHandler = Some(|lex| {
    match_label!(lex [
        [ a
            [ t c h     => KeywordReserved ]     // match
            [ p p i n g => KeywordMapping ]
        ]
        [ e m o r y     => KeywordMemory ]
        [ i n u t e s   => UnitTime ]            // minutes
        [ o d i f i e r => DeclarationModifier ]
        [ s g           => IdentifierBuiltin ]   // msg
        [ u l m o d     => IdentifierBuiltin ]   // mulmod
    ]);

    lex.read_label();
    lex.token = Identifier;
});

// Identifier or keyword starting with a letter `n`
pub const L_N: ByteHandler = Some(|lex| {
    match_label!(lex [
        [ e w   => KeywordNew ]
        [ o w   => IdentifierBuiltin ] // now
        [ u l l => KeywordReserved ]   // null
    ]);

    lex.read_label();
    lex.token = Identifier;
});


// Identifier or keyword starting with a letter `n`
pub const L_O: ByteHandler = Some(|lex| {
    match_label!(lex [ f => KeywordReserved ]); // of

    lex.read_label();
    lex.token = Identifier;
});

// Identifier or keyword starting with a letter `p`
pub const L_P: ByteHandler = Some(|lex| {
    match_label!(lex [
        [ a y a b l e   => KeywordPayable ]
        [ u
            [ r e       => KeywordPure ]
            [ b l i c   => KeywordPublic ]
        ]
        [ r
            [ a g m a   => KeywordPragma ]
            [ i v a t e => KeywordPrivate ]
        ]
    ]);

    lex.read_label();
    lex.token = Identifier;
});

// Identifier or keyword starting with a letter `r`
pub const L_R: ByteHandler = Some(|lex| {
    match_label!(lex [
        [ e
            [ t u r n
                [   => KeywordReturn ]
                [ s => KeywordReturns ]
            ]
            [ l o c a t a b l e => KeywordReserved ]   // relocatable
            [ v e r t           => IdentifierBuiltin ] // revert
            [ q u i r e         => IdentifierBuiltin ] // require
        ]
        [ i p e m d _1 _6 _0    => IdentifierBuiltin ] // ripemd160
    ]);

    lex.read_label();
    lex.token = Identifier;
});

// Identifier or keyword starting with a letter `s`
pub const L_S: ByteHandler = Some(|lex| {
    match_label!(lex [
        [ e
            [ c o n d s           => UnitTime ]          // seconds
            [ l f d e s t r u c t => IdentifierBuiltin ] // selfdestruct
        ]
        [ h a
            [_3       => IdentifierBuiltin]    // sha3
            [_2 _5 _6 => IdentifierBuiltin]    // sha256
        ]
        [ t
            [ a t i c   => KeywordReserved ]   // static
            [ o r a g e => KeywordStorage ]
            [ r
                [ u c t => DeclarationStruct ]
                [ i n g => KeywordType ]       // string
            ]
        ]
        [ u
            [ p e r     => KeywordSuper ]      // super
            [ i c i d e => IdentifierBuiltin ] // suicide
        ]
        [ w i t c h     => KeywordReserved ]   // switch
        [ z a b o       => UnitEther ]         // szabo
    ]);

    lex.read_label();
    lex.token = Identifier;
});

// Identifier or keyword starting with a letter `t`
pub const L_T: ByteHandler = Some(|lex| {
    match_label!(lex [
        [ y p e
            [     => KeywordReserved ] // type
            [ o f => KeywordReserved ] // typeof
        ]
        [ h
            [ i s   => KeywordThis ]
            [ r o w => KeywordThrow ]
        ]
        [ r
            [ y   => KeywordReserved ] // try
            [ u e => LiteralTrue ]
        ]
        [ x       => IdentifierBuiltin ] // tx
    ]);

    lex.read_label();
    lex.token = Identifier;
});

// Identifier or keyword starting with a letter `u`
pub const L_U: ByteHandler = Some(|lex| {
    match_label!(lex [
        [ s i n g                        => KeywordUsing ]
        [ i n t
            [                            => KeywordType ]
            [ (b'0'...b'9') @ bits256    => KeywordType ]
        ]
        [ f i x e d
            [                            => KeywordType ]
            [ (b'0'...b'9') @ bits256x80 => KeywordType ]
        ]
    ]);

    lex.read_label();
    lex.token = Identifier;
});

// Identifier or keyword starting with a letter `v`
pub const L_V: ByteHandler = Some(|lex| {
    match_label!(lex [
        [ a r   => DeclarationVar ]
        [ i e w => KeywordView ]
    ]);

    lex.read_label();
    lex.token = Identifier;
});

// Identifier or keyword starting with a letter `w`
pub const L_W: ByteHandler = Some(|lex| {
    match_label!(lex [
        [ h i l e => KeywordWhile ]
        [ e
            [ e k s => UnitTime ]  // weeks
            [ i     => UnitEther ] // wei
        ]
    ]);

    lex.read_label();
    lex.token = Identifier;
});

// // Identifier or keyword starting with a letter `y`
// pub const L_Y: ByteHandler = Some(|lex| {
//     match_label!(lex [i e l d => Yield]);

//     lex.read_label();
//     lex.token = Identifier;
// });
