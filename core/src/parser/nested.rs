use parser::Parser;
use lexer::Token;
use ast::*;

const TOTAL_TOKENS: usize = 113;

type NestedHandler = Option<for<'ast> fn(&mut Parser<'ast>, ExpressionNode<'ast>) -> Option<ExpressionNode<'ast>>>;

pub trait Precedence {
    const LUT: [NestedHandler; TOTAL_TOKENS];

    #[inline]
    fn get_handler(token: Token) -> NestedHandler {
        Self::LUT[token as usize]
    }
}

macro_rules! precedence {
    ($name:ident, $table:tt) => {
        pub struct $name;

        impl Precedence for $name {
            const LUT: [NestedHandler; TOTAL_TOKENS] = $table;
        }
    }
}

precedence!(TopPrecedence, [
    _____, _____, _____, _____, MEMBR, CALL,  _____, _____, _____, INDEX, _____, _____,
//  EOF    ;      :      ,      .      (      )      {      }      [      ]      =>

    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
//  IDENT  BLTIN  CONTR  LIB    IFACE  ENUM   STRUCT MODIF  EVENT  FUNCT  VAR    ANON

    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
//  AS     ASM    BREAK  CONST  CONTIN DO     DELETE ELSE   EXTERN FOR    HEX    IF

    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
//  INDEX  INTERN IMPORT IS     MAP    MEM    NEW    PAY    PULIC  PRAGMA PRIV   PURE

    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
//  RET    RETNS  STORAG SUPER  THIS   THROW  USING  VIEW   WHILE  RESERV T_BOOL T_ADDR

    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
//  T_STR  T_BYT  T_INT  T_UINT T_FIX  T_UFIX L_TRUE L_FALS L_HEX  L_INT  L_RAT  L_STR

    _____, _____, _____, _____, INC,   DEC,   _____, _____, MUL,   DIV,   REM,   EXPN,
//  U_ETH  U_TIME :=     =:     ++     --     !      ~      *      /      %      **

    ADD,   SUB,   BSL,   BSR,   LESS,  LSEQ,  GRTR,  GREQ,  EQ,    INEQ,  B_AND, B_XOR,
//  +      -      <<     >>     <      <=     >      >=     ==     !=     &      ^

    B_OR,  AND,   OR,    COND,  ASGN,  A_ADD, A_SUB, A_MUL, A_DIV, A_REM, A_BSL, A_BSR,
//  |      &&     ||     ?      =      +=     -=     *=     /=     %=     <<=    >>=

    A_BAN, A_XOR, A_BOR, _____, _____,
//  &=     ^=     |=     ERRTOK ERREOF
]);

precedence!(Precedence14, [
    _____, _____, _____, _____, MEMBR, CALL,  _____, _____, _____, INDEX, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, INC,   DEC,   _____, _____, MUL,   DIV,   REM,   EXPN,
    ADD,   SUB,   BSL,   BSR,   LESS,  LSEQ,  GRTR,  GREQ,  EQ,    INEQ,  B_AND, B_XOR,
    B_OR,  AND,   OR,    COND,  _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____,
]);

precedence!(Precedence13, [
    _____, _____, _____, _____, MEMBR, CALL,  _____, _____, _____, INDEX, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, INC,   DEC,   _____, _____, MUL,   DIV,   REM,   EXPN,
    ADD,   SUB,   BSL,   BSR,   LESS,  LSEQ,  GRTR,  GREQ,  EQ,    INEQ,  B_AND, B_XOR,
    B_OR,  AND,   OR,    _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____,
]);

precedence!(Precedence12, [
    _____, _____, _____, _____, MEMBR, CALL,  _____, _____, _____, INDEX, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, INC,   DEC,   _____, _____, MUL,   DIV,   REM,   EXPN,
    ADD,   SUB,   BSL,   BSR,   LESS,  LSEQ,  GRTR,  GREQ,  EQ,    INEQ,  B_AND, B_XOR,
    B_OR,  AND,   _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____,
]);

precedence!(Precedence11, [
    _____, _____, _____, _____, MEMBR, CALL,  _____, _____, _____, INDEX, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, INC,   DEC,   _____, _____, MUL,   DIV,   REM,   EXPN,
    ADD,   SUB,   BSL,   BSR,   LESS,  LSEQ,  GRTR,  GREQ,  EQ,    INEQ,  B_AND, B_XOR,
    B_OR,  _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____,
]);

precedence!(Precedence10, [
    _____, _____, _____, _____, MEMBR, CALL,  _____, _____, _____, INDEX, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, INC,   DEC,   _____, _____, MUL,   DIV,   REM,   EXPN,
    ADD,   SUB,   BSL,   BSR,   LESS,  LSEQ,  GRTR,  GREQ,  _____, _____, B_AND, B_XOR,
    B_OR,  _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____,
]);

precedence!(Precedence9, [
    _____, _____, _____, _____, MEMBR, CALL,  _____, _____, _____, INDEX, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, INC,   DEC,   _____, _____, MUL,   DIV,   REM,   EXPN,
    ADD,   SUB,   BSL,   BSR,   _____, _____, _____, _____, _____, _____, B_AND, B_XOR,
    B_OR,  _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____,
]);

precedence!(Precedence8, [
    _____, _____, _____, _____, MEMBR, CALL,  _____, _____, _____, INDEX, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, INC,   DEC,   _____, _____, MUL,   DIV,   REM,   EXPN,
    ADD,   SUB,   BSL,   BSR,   _____, _____, _____, _____, _____, _____, B_AND, B_XOR,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____,
]);

precedence!(Precedence7, [
    _____, _____, _____, _____, MEMBR, CALL,  _____, _____, _____, INDEX, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, INC,   DEC,   _____, _____, MUL,   DIV,   REM,   EXPN,
    ADD,   SUB,   BSL,   BSR,   _____, _____, _____, _____, _____, _____, B_AND, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____,
]);

precedence!(Precedence6, [
    _____, _____, _____, _____, MEMBR, CALL,  _____, _____, _____, INDEX, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, INC,   DEC,   _____, _____, MUL,   DIV,   REM,   EXPN,
    ADD,   SUB,   BSL,   BSR,   _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____,
]);

precedence!(Precedence5, [
    _____, _____, _____, _____, MEMBR, CALL,  _____, _____, _____, INDEX, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, INC,   DEC,   _____, _____, MUL,   DIV,   REM,   EXPN,
    ADD,   SUB,   _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____,
]);

precedence!(Precedence4, [
    _____, _____, _____, _____, MEMBR, CALL,  _____, _____, _____, INDEX, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, INC,   DEC,   _____, _____, MUL,   DIV,   REM,   EXPN,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____,
]);

precedence!(Precedence3, [
    _____, _____, _____, _____, MEMBR, CALL,  _____, _____, _____, INDEX, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, INC,   DEC,   _____, _____, _____, _____, _____, EXPN,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____,
]);

// TODO: Use in prefix expressions
precedence!(Precedence2, [
    _____, _____, _____, _____, MEMBR, CALL,  _____, _____, _____, INDEX, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, INC,   DEC,   _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
    _____, _____, _____, _____, _____,
]);

const _____: NestedHandler = None;

const CALL: NestedHandler = Some(|par, callee| {
    par.lexer.consume();

    par.expect(Token::ParenOpen);

    let arguments = par.expression_list();
    let end       = par.expect_end(Token::ParenClose);

    par.node_at(callee.start, end, CallExpression {
        callee,
        arguments,
    })
});

const MEMBR: NestedHandler = Some(|par, object| {
    par.lexer.consume();

    let member = par.expect_str_node(Token::Identifier);

    par.node_at(object.start, member.end, MemberAccessExpression {
        object,
        member,
    })
});

const INDEX: NestedHandler = Some(|par, array| {
    par.lexer.consume();

    let index = par.expression::<TopPrecedence>();
    let end   = par.expect_end(Token::BracketClose);

    par.node_at(array.start, end, IndexAccessExpression {
        array,
        index,
    })
});

const INC: NestedHandler = Some(|par, operand| {
    let operator: Node<_> = par.node_at_token(PostfixOperator::Increment);

    par.node_at(operand.start, operator.end, PostfixExpression {
        operator,
        operand,
    })
});

const DEC: NestedHandler = Some(|par, operand| {
    let operator: Node<_> = par.node_at_token(PostfixOperator::Decrement);

    par.node_at(operand.start, operator.end, PostfixExpression {
        operator,
        operand,
    })
});

const COND: NestedHandler = Some(|par, test| {
    par.lexer.consume();

    let consequent = expect!(par, par.expression::<Precedence14>());

    par.expect(Token::Colon);

    let alternate = expect!(par, par.expression::<Precedence14>());

    par.node_at(test.start, alternate.end, ConditionalExpression {
        test,
        consequent,
        alternate,
    })
});


macro_rules! assign {
    ($name:ident => $op:ident) => {
        const $name: NestedHandler = {
            fn handler<'ast>(par: &mut Parser<'ast>, left: ExpressionNode<'ast>) -> Option<ExpressionNode<'ast>> {
                // TODO: check if left is LValue

                let operator = par.node_at_token(AssignmentOperator::$op);
                let right    = expect!(par, par.expression::<TopPrecedence>());

                par.node_at(left.start, right.end, AssignmentExpression {
                    operator,
                    left,
                    right,
                })
            }

            Some(handler)
        };
    }
}

macro_rules! binary {
    ($name:ident, $precedence:ident => $op:ident) => {
        const $name: NestedHandler = {
            fn handler<'ast>(par: &mut Parser<'ast>, left: ExpressionNode<'ast>) -> Option<ExpressionNode<'ast>> {
                let operator = par.node_at_token(BinaryOperator::$op);
                let right    = expect!(par, par.expression::<$precedence>());

                par.node_at(left.start, right.end, BinaryExpression {
                    operator,
                    left,
                    right,
                })
            }

            Some(handler)
        };
    }
}

assign!(ASGN  => Plain);
assign!(A_ADD => Addition);
assign!(A_SUB => Subtraction);
assign!(A_MUL => Multiplication);
assign!(A_DIV => Division);
assign!(A_REM => Remainder);
assign!(A_BSL => BitShiftLeft);
assign!(A_BSR => BitShiftRight);
assign!(A_BAN => BitAnd);
assign!(A_XOR => BitXor);
assign!(A_BOR => BitOr);

binary!(OR    , Precedence13 => LogicalOr);
binary!(AND   , Precedence12 => LogicalAnd);
binary!(EQ    , Precedence11 => Equality);
binary!(INEQ  , Precedence11 => Inequality);
binary!(LESS  , Precedence10 => Lesser);
binary!(LSEQ  , Precedence10 => LesserEquals);
binary!(GRTR  , Precedence10 => Greater);
binary!(GREQ  , Precedence10 => GreaterEquals);
binary!(B_OR  , Precedence9  => BitOr);
binary!(B_XOR , Precedence8  => BitXor);
binary!(B_AND , Precedence7  => BitAnd);
binary!(BSL   , Precedence6  => BitShiftLeft);
binary!(BSR   , Precedence6  => BitShiftRight);
binary!(ADD   , Precedence5  => Addition);
binary!(SUB   , Precedence5  => Subtraction);
binary!(MUL   , Precedence4  => Multiplication);
binary!(DIV   , Precedence4  => Division);
binary!(REM   , Precedence4  => Remainder);
binary!(EXPN  , Precedence3  => Exponent);


impl<'ast> Parser<'ast> {
    #[inline]
    pub fn nested_expression<P>(&mut self, mut left: ExpressionNode<'ast>) -> ExpressionNode<'ast>
    where
        P: Precedence,
    {
        loop {
            match P::get_handler(self.lexer.token).and_then(|handler| handler(self, left)) {
                Some(node) => left = node,
                None       => break left,
            }
        }
    }
}
