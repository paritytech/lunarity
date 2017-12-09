use toolshed::list::ListBuilder;
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
    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
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

    B_OR,  AND,   OR,    _____, _____, _____, _____, _____, _____, _____, _____, _____,
//  |      &&     ||     ?      =      +=     -=     *=     /=     %=     <<=    >>=

    _____, _____, _____, _____, _____,
//  &=     ^=     |=     ERRTOK ERREOF
]);

const _____: NestedHandler = None;

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


macro_rules! binary {
    ($name:ident, $precedence:ident => $op:expr) => {
        const $name: NestedHandler = {
            fn handler<'ast>(par: &mut Parser<'ast>, left: ExpressionNode<'ast>) -> Option<ExpressionNode<'ast>> {
                let operator = par.node_at_token($op);
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

binary!(OR    , TopPrecedence => BinaryOperator::LogicalOr);      // TODO: Precedence13
binary!(AND   , TopPrecedence => BinaryOperator::LogicalAnd);     // TODO: Precedence12
binary!(EQ    , TopPrecedence => BinaryOperator::Equality);       // TODO: Precedence11
binary!(INEQ  , TopPrecedence => BinaryOperator::Inequality);     // TODO: Precedence11
binary!(LESS  , TopPrecedence => BinaryOperator::Lesser);         // TODO: Precedence10
binary!(LSEQ  , TopPrecedence => BinaryOperator::LesserEquals);   // TODO: Precedence10
binary!(GRTR  , TopPrecedence => BinaryOperator::Greater);        // TODO: Precedence10
binary!(GREQ  , TopPrecedence => BinaryOperator::GreaterEquals);  // TODO: Precedence10
binary!(B_OR  , TopPrecedence => BinaryOperator::BitOr);          // TODO: Precedence9
binary!(B_XOR , TopPrecedence => BinaryOperator::BitXor);         // TODO: Precedence8
binary!(B_AND , TopPrecedence => BinaryOperator::BitAnd);         // TODO: Precedence7
binary!(BSL   , TopPrecedence => BinaryOperator::BitShiftLeft);   // TODO: Precedence6
binary!(BSR   , TopPrecedence => BinaryOperator::BitShiftRight);  // TODO: Precedence6
binary!(ADD   , TopPrecedence => BinaryOperator::Addition);       // TODO: Precedence5
binary!(SUB   , TopPrecedence => BinaryOperator::Subtraction);    // TODO: Precedence5
binary!(MUL   , TopPrecedence => BinaryOperator::Multiplication); // TODO: Precedence4
binary!(DIV   , TopPrecedence => BinaryOperator::Division);       // TODO: Precedence4
binary!(REM   , TopPrecedence => BinaryOperator::Remainder);      // TODO: Precedence4
binary!(EXPN  , TopPrecedence => BinaryOperator::Exponent);       // TODO: Precedence3


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
