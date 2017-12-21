use toolshed::list::ListBuilder;

use ast::*;
use parser::{Parser, Precedence, Precedence2, TopPrecedence, StatementContext};
use lexer::Token;

type ExpressionHandler = for<'ast> fn(&mut Parser<'ast>) -> Option<ExpressionNode<'ast>>;

static LUT: [ExpressionHandler; 121] = [
    _____, _____, _____, _____, _____, TUPLE, _____, _____, _____, _____, _____, _____,
//  EOF    ;      :      ,      .      (      )      {      }      [      ]      =>

    IDENT, IDENT, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
//  IDENT  BLTIN  CONTR  LIB    IFACE  ENUM   STRUCT MODIF  EVENT  FUNCT  VAR    ANON

    _____, _____, _____, _____, _____, _____, DELET, _____, _____, _____, _____, _____,
//  AS     ASM    BREAK  CONST  CONTIN DO     DELETE ELSE   EXTERN FOR    HEX    IF

    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
//  INDEX  INTERN IMPORT IS     MAP    MEM    NEW    PAY    PULIC  PRAGMA PRIV   PURE

    _____, _____, _____, _____, THIS,  _____, _____, _____, _____, _____, BOOL,  ADDR,
//  RET    RETNS  STORAG SUPER  THIS   THROW  USING  VIEW   WHILE  RESERV T_BOOL T_ADDR

    STR,   BYTE,  INT,   UINT,  FIXED, UFIXD, TRUE,  FALSE, L_HEX, L_INT, L_RAT, L_STR,
//  T_STR  T_BYT  T_INT  T_UINT T_FIX  T_UFIX L_TRUE L_FALS L_HEX  L_INT  L_RAT  L_STR

    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
//  E_ETH  E_FINN E_SZAB E_WEI  T_YEAR T_WEEK T_DAYS T_HOUR T_MIN  T_SEC  :=     =:

    INC,   DEC,   NOT,   B_NOT, _____, _____, _____, _____, PLUS,  MIN, _____, _____,
//  ++     --     !      ~      *      /      %      **     +      -      <<     >>

    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
//  <      <=     >      >=     ==     !=     &      ^      |      &&     ||     ?

    _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____, _____,
//  =      +=     -=     *=     /=     %=     <<=    >>=    &=     ^=     |=     ERRTOK

    _____,
//  ERREOF
];


macro_rules! create_handlers {
    ($( const $name:ident = |$par:tt| $code:expr; )* $( pub const $pname:ident = |$ppar:ident| $pcode:expr; )*) => {
        $(
            #[allow(non_snake_case)]
            fn $name<'ast>($par: &mut Parser<'ast>) -> Option<ExpressionNode<'ast>> {
                $code
            }
        )*

        pub(crate) mod statements {
            use super::*;

            $(
                #[allow(non_snake_case)]
                pub fn $pname<'ast, Context: StatementContext>($ppar: &mut Parser<'ast>, _ctx: Context) -> Option<StatementNode<'ast>> {
                    let expression: Option<Node<_>> = $pcode;
                    let expression = $ppar.nested_expression::<TopPrecedence>(expression?);
                    $ppar.wrap_expression(expression)
                }
            )*
        }

        $(
            #[allow(non_snake_case)]
            fn $pname<'ast>($ppar: &mut Parser<'ast>) -> Option<ExpressionNode<'ast>> {
                $pcode
            }
        )*
    };
}

create_handlers! {
    const _____ = |_| None;

    const BOOL = |par| par.bool_type_name();

    const ADDR = |par| par.address_type_name();

    const STR = |par| par.string_type_name();

    const BYTE = |par| par.byte_type_name();

    const INT = |par| par.int_type_name();

    const UINT = |par| par.uint_type_name();

    const FIXED = |par| par.fixed_type_name();

    const UFIXD = |par| par.ufixed_type_name();

    pub const IDENT = |par| par.identifier_expression();

    pub const THIS = |par| par.node_at_token(ThisExpression);

    pub const TUPLE = |par| par.tuple_expression();

    pub const NOT = |par| par.prefix_expression(PrefixOperator::LogicalNot);

    pub const B_NOT = |par| par.prefix_expression(PrefixOperator::BitNot);

    pub const DELET = |par| par.prefix_expression(PrefixOperator::Delete);

    pub const INC = |par| par.prefix_expression(PrefixOperator::Increment);

    pub const DEC = |par| par.prefix_expression(PrefixOperator::Decrement);

    pub const PLUS = |par| par.prefix_expression(PrefixOperator::Plus);

    pub const MIN = |par| par.prefix_expression(PrefixOperator::Minus);

    pub const TRUE = |par| par.node_at_token(Primitive::Bool(true));

    pub const FALSE = |par| par.node_at_token(Primitive::Bool(false));

    pub const L_HEX = |par| {
        let number = par.lexer.token_as_str();

        par.node_at_token(Primitive::HexNumber(number))
    };

    pub const L_INT = |par| par.integer_number();

    pub const L_RAT = |par| {
        let number = par.lexer.token_as_str();

        par.node_at_token(Primitive::RationalNumber(number))
    };

    pub const L_STR = |par| {
        let string = par.lexer.token_as_str();

        par.node_at_token(Primitive::String(string))
    };
}

impl<'ast> Parser<'ast> {
    #[inline]
    pub fn expression<P>(&mut self) -> Option<ExpressionNode<'ast>>
    where
        P: Precedence
    {
        self.bound_expression()
            .map(|expression| self.nested_expression::<P>(expression))
    }

    fn bound_expression(&mut self) -> Option<ExpressionNode<'ast>> {
        LUT[self.lexer.token as usize](self)
    }

    pub fn identifier_expression(&mut self) -> Option<ExpressionNode<'ast>> {
        let ident = self.lexer.token_as_str();

        self.node_at_token(ident)
    }

    #[inline]
    pub fn expression_list(&mut self) -> ExpressionList<'ast> {
        let builder = match self.expression::<TopPrecedence>() {
            Some(expression) => ListBuilder::new(self.arena, expression),
            None             => return NodeList::empty(),
        };

        while self.allow(Token::Comma) {
            match self.expression::<TopPrecedence>() {
                Some(expression) => builder.push(self.arena, expression),
                None             => self.error(),
            }
        }

        builder.as_list()
    }

    fn tuple_expression(&mut self) -> Option<ExpressionNode<'ast>> {
        let start       = self.lexer.start_then_consume();
        let expressions = self.expression_list();
        let end         = self.expect_end(Token::ParenClose);

        self.node_at(start, end, TupleExpression {
            expressions,
        })
    }

    fn prefix_expression(&mut self, operator: PrefixOperator) -> Option<ExpressionNode<'ast>> {
        let operator: Node<_> = self.node_at_token(operator);
        let operand = expect!(self, self.expression::<Precedence2>());

        self.node_at(operator.start, operand.end, PrefixExpression {
            operator,
            operand,
        })
    }

    fn integer_number(&mut self) -> Option<ExpressionNode<'ast>> {
        let number = self.lexer.token_as_str();
        let (start, end) = self.lexer.loc();

        self.lexer.consume();

        let unit = match self.lexer.token {
            Token::UnitEther       => NumberUnit::Ether(EtherUnit::Ether),
            Token::UnitFinney      => NumberUnit::Ether(EtherUnit::Finney),
            Token::UnitSzabo       => NumberUnit::Ether(EtherUnit::Szabo),
            Token::UnitWei         => NumberUnit::Ether(EtherUnit::Wei),
            Token::UnitTimeYears   => NumberUnit::Time(TimeUnit::Years),
            Token::UnitTimeWeeks   => NumberUnit::Time(TimeUnit::Weeks),
            Token::UnitTimeDays    => NumberUnit::Time(TimeUnit::Days),
            Token::UnitTimeHours   => NumberUnit::Time(TimeUnit::Hours),
            Token::UnitTimeMinutes => NumberUnit::Time(TimeUnit::Minutes),
            Token::UnitTimeSeconds => NumberUnit::Time(TimeUnit::Seconds),

            _ => return self.node_at(start, end, Primitive::IntegerNumber(number, NumberUnit::None)),
        };

        let end = self.lexer.end();
        self.lexer.consume();

        self.node_at(start, end, Primitive::IntegerNumber(number, unit))
    }
}


#[cfg(test)]
mod test {
    use super::*;
    use parser::mock::{Mock, assert_units};

    #[test]
    fn nested_expressions() {
        let m = Mock::new();

        assert_units(r#"

            contract Foo {
                function() {
                    !doge;
                    ~doge;
                    delete doge;
                    ++doge;
                    --doge;
                    +doge;
                    -doge;
                }
            }

        "#, [
            m.node(14, 286, ContractDefinition {
                name: m.node(23, 26, "Foo"),
                inherits: NodeList::empty(),
                body: m.list([
                    m.node(45, 272, FunctionDefinition {
                        name: None,
                        params: NodeList::empty(),
                        visibility: None,
                        mutability: None,
                        modifiers: NodeList::empty(),
                        returns: NodeList::empty(),
                        block: m.node(56, 272, Block {
                            body: m.list([
                                m.stmt_expr(78, 83, 84, PrefixExpression {
                                    operator: m.node(78, 79, PrefixOperator::LogicalNot),
                                    operand: m.node(79, 83, "doge"),
                                }),
                                m.stmt_expr(105, 110, 111, PrefixExpression {
                                    operator: m.node(105, 106, PrefixOperator::BitNot),
                                    operand: m.node(106, 110, "doge"),
                                }),
                                m.stmt_expr(132, 143, 144, PrefixExpression {
                                    operator: m.node(132, 138, PrefixOperator::Delete),
                                    operand: m.node(139, 143, "doge"),
                                }),
                                m.stmt_expr(165, 171, 172, PrefixExpression {
                                    operator: m.node(165, 167, PrefixOperator::Increment),
                                    operand: m.node(167, 171, "doge"),
                                }),
                                m.stmt_expr(193, 199, 200, PrefixExpression {
                                    operator: m.node(193, 195, PrefixOperator::Decrement),
                                    operand: m.node(195, 199, "doge"),
                                }),
                                m.stmt_expr(221, 226, 227, PrefixExpression {
                                    operator: m.node(221, 222, PrefixOperator::Plus),
                                    operand: m.node(222, 226, "doge"),
                                }),
                                m.stmt_expr(248, 253, 254, PrefixExpression {
                                    operator: m.node(248, 249, PrefixOperator::Minus),
                                    operand: m.node(249, 253, "doge"),
                                }),
                            ]),
                        }),
                    }),
                ]),
            }),
        ]);
    }
}
