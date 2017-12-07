use lexer::{ByteHandler, Lexer};
use lexer::util::legal_in_label;
use lexer::token::Token;
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
    // Run a modifier after
    ($lex:ident [ $( $byte:ident )* @ $mod:ident => $token:expr ]) => {
        if $lex.$mod($token) {
            return;
        }
    };

    // Parse a sequence of label bytes
    ($lex:ident [ $( $byte:ident )* => $token:expr ]) => {
        if
            $( $lex.next_byte() == byte!($byte) && )* { $lex.bump(); $lex.end_of_label() }
        {
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
            ch if !legal_in_label(ch) => return $lex.token = $token,
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
        [ b s t r a c t   => ReservedWord ]    // abstract
        [ d d
            [ r e s s     => TypeAddress ]
            [ m o d       => IdentifierBuiltin ]
        ]
        [ f t e r         => ReservedWord ]    // after
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
        [ o o l   => TypeBool ]
        [ y t e
            [     => { lex.type_size.0 = 1; TypeByte } ]
            [ s
                [                         => { lex.type_size.0 = 32; TypeByte } ]
                [ (b'1'...b'9') @ bytes32 => TypeByte ]
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
            [ s e    => ReservedWord ] // case
            [ t c h  => ReservedWord ] // catch
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
        [ a y s         => UnitTime ]  // days
        [ o             => KeywordDo ]
        [ e
            [ l e t e   => KeywordDelete ]
            [ f a u l t => ReservedWord ] // default
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
        [ i
            [ n
                [ a l   => ReservedWord ] // final
                [ n e y => UnitEther ]    // finney
            ]
            [ x e d
                [                            => { lex.type_size = (32, 80); TypeFixed } ]
                [ (b'1'...b'9') @ bits256x80 => TypeFixed ]
            ]
        ]
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
            [               => ReservedWord ] // in
            [ l i n e       => ReservedWord ] // inline
            [ t
                [             => { lex.type_size.0 = 32; TypeInt } ] // int
                [ e r
                    [ f a c e => DeclarationInterface ]
                    [ n a l   => KeywordInternal ]
                ]
                [ (b'1'...b'9') @ bits256 => TypeInt ]
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
        [ e t         => ReservedWord ] // let
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
            [ t c h     => ReservedWord ]     // match
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
        [ u l l => ReservedWord ]   // null
    ]);

    lex.read_label();
    lex.token = Identifier;
});


// Identifier or keyword starting with a letter `n`
pub const L_O: ByteHandler = Some(|lex| {
    match_label!(lex [ f => ReservedWord ]); // of

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
            [ l o c a t a b l e => ReservedWord ]   // relocatable
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
            [ a t i c   => ReservedWord ]   // static
            [ o r a g e => KeywordStorage ]
            [ r
                [ u c t => DeclarationStruct ]
                [ i n g => TypeString ]
            ]
        ]
        [ u
            [ p e r     => KeywordSuper ]      // super
            [ i c i d e => IdentifierBuiltin ] // suicide
        ]
        [ w i t c h     => ReservedWord ]      // switch
        [ z a b o       => UnitEther ]         // szabo
    ]);

    lex.read_label();
    lex.token = Identifier;
});

// Identifier or keyword starting with a letter `t`
pub const L_T: ByteHandler = Some(|lex| {
    match_label!(lex [
        [ y p e
            [     => ReservedWord ] // type
            [ o f => ReservedWord ] // typeof
        ]
        [ h
            [ i s   => KeywordThis ]
            [ r o w => KeywordThrow ]
        ]
        [ r
            [ y   => ReservedWord ] // try
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
            [                            => { lex.type_size.0 = 32; TypeUint } ]
            [ (b'1'...b'9') @ bits256    => TypeUint ]
        ]
        [ f i x e d
            [                            => { lex.type_size = (32, 80); TypeUfixed } ]
            [ (b'1'...b'9') @ bits256x80 => TypeUfixed ]
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

// Identifier or keyword starting with a letter `y`
pub const L_Y: ByteHandler = Some(|lex| {
    match_label!(lex [ e a r s => UnitTime ]); // years

    lex.read_label();
    lex.token = Identifier;
});

impl<'arena> Lexer<'arena> {
    #[inline]
    fn end_of_label(&self) -> bool {
        !legal_in_label(self.read_byte())
    }

    fn bytes32(&mut self, token: Token) -> bool {
         let mut n = self.read_byte() - b'0';

        match self.next_byte() {
            byte @ b'0'...b'9' => {
                n = n * 10 + (byte - b'0');

                self.bump();
            },
            _ => {},
        }

        // First digit has to be 1...9, so we don't need to check for zero.
        if n <= 32 && self.end_of_label() {
            self.token       = token;
            self.type_size.0 = n;

            return true;
        }

        false
    }

    fn bits256(&mut self, token: Token) -> bool {
        let mut n = (self.read_byte() - b'0') as u16;

        self.bump();

        for _ in 0..2 {
            match self.read_byte() {
                byte @ b'0'...b'9' => {
                    n = n * 10 + (byte - b'0') as u16;
                },
                _ => break,
            }

            self.bump();
        }


        // First digit has to be 1...9, so we don't need to check for zero.
        if n % 8 == 0 && n <= 256 && self.end_of_label() {
            self.token       = token;
            self.type_size.0 = (n / 8) as u8;

            return true;
        }

        false
    }

    fn bits256x80(&mut self, token: Token) -> bool {
        let mut m = (self.read_byte() - b'0') as u16;
        let mut byte = self.next_byte();

        for _ in 0..2 {
            match byte {
                b'0'...b'9' => {},
                _ => break,
            }

            m = m * 10 + (byte - b'0') as u16;
            byte = self.next_byte();
        }

        // First digit has to be 1...9, so we don't need to check for zero.
        if m % 8 != 0 || m > 256 || byte != b'x' {
            return false;
        }

        let mut n = match self.next_byte() {
            byte @ b'0'...b'9' => (byte - b'0'),
            _                  => return false,
        };

        match self.next_byte() {
            byte @ b'0'...b'9' => {
                n = n * 10 + (byte - b'0');

                self.bump();
            },
            _ => {},
        }

        if n <= 80 && self.end_of_label() {
            self.token     = token;
            self.type_size = ((m / 8) as u8, n);

            return true;
        }

        false
    }
}
