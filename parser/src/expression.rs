use toolshed::list::ListBuilder;

use ast::*;
use {Parser, Precedence, P2, TOP};
use lexer::{Token, Logos, lookup};

type HandlerFn = for<'ast> fn(&mut Parser<'ast>) -> Option<ExpressionNode<'ast>>;

static EXPRESSION_LUT: [HandlerFn; Token::SIZE] = lookup! {
    Token::KeywordThis         => |par| par.node_at_token(ThisExpression),
    Token::Identifier          => |par| par.node_from_slice(|ident| ident),
    Token::IdentifierBuiltin   => |par| par.node_from_slice(|ident| ident),
    Token::ParenOpen           => |par| par.tuple_expression(),
    Token::OperatorLogicalNot  => |par| par.prefix_expression(PrefixOperator::LogicalNot),
    Token::OperatorBitNot      => |par| par.prefix_expression(PrefixOperator::BitNot),
    Token::KeywordDelete       => |par| par.prefix_expression(PrefixOperator::Delete),
    Token::OperatorIncrement   => |par| par.prefix_expression(PrefixOperator::Increment),
    Token::OperatorDecrement   => |par| par.prefix_expression(PrefixOperator::Decrement),
    Token::OperatorAddition    => |par| par.prefix_expression(PrefixOperator::Plus),
    Token::OperatorSubtraction => |par| par.prefix_expression(PrefixOperator::Minus),
    Token::LiteralTrue         => |par| par.node_at_token(Primitive::Bool(true)),
    Token::LiteralFalse        => |par| par.node_at_token(Primitive::Bool(false)),
    Token::LiteralHex          => |par| par.node_from_slice(|slice| Primitive::HexNumber(slice)),
    Token::LiteralInteger      => |par| par.integer_number(),
    Token::LiteralRational     => |par| par.node_from_slice(|slice| Primitive::RationalNumber(slice)),
    Token::LiteralString       => |par| par.node_from_slice(|slice| Primitive::String(slice)),
    Token::TypeBool            => |par| par.node_at_token(ElementaryTypeName::Bool),
    Token::TypeAddress         => |par| par.node_at_token(ElementaryTypeName::Address),
    Token::TypeString          => |par| par.node_at_token(ElementaryTypeName::String),
    Token::TypeByte            => |par| {
        let size = par.lexer.extras.0;

        par.node_at_token(ElementaryTypeName::Byte(size))
    },
    Token::TypeBytes => |par| {
        par.node_at_token(ElementaryTypeName::Bytes)
    },
    Token::TypeInt => |par| {
        let size = par.lexer.extras.0;

        par.node_at_token(ElementaryTypeName::Int(size))
    },
    Token::TypeUint => |par| {
        let size = par.lexer.extras.0;

        par.node_at_token(ElementaryTypeName::Uint(size))
    },
    Token::TypeFixed => |par| {
        let size = par.lexer.extras;

        par.node_at_token(ElementaryTypeName::Fixed(size.0, size.1))
    },
    Token::TypeUfixed => |par| {
        let size = par.lexer.extras;

        par.node_at_token(ElementaryTypeName::Ufixed(size.0, size.1))
    },
    _ => |_| None,
};

impl<'ast> Parser<'ast> {
    #[inline]
    pub fn expression(&mut self, precedence: Precedence) -> Option<ExpressionNode<'ast>> {
        EXPRESSION_LUT[self.lexer.token as usize](self)
            .map(|expression| self.nested_expression(expression, precedence))
    }

    #[inline]
    pub fn expression_list(&mut self) -> ExpressionList<'ast> {
        let builder = match self.expression(TOP) {
            Some(expression) => ListBuilder::new(self.arena, expression),
            None             => return NodeList::empty(),
        };

        while self.allow(Token::Comma) {
            match self.expression(TOP) {
                Some(expression) => builder.push(self.arena, expression),
                None             => self.error(),
            }
        }

        builder.as_list()
    }

    fn tuple_expression(&mut self) -> Option<ExpressionNode<'ast>> {
        let start       = self.start_then_advance();
        let expressions = self.expression_list();
        let end         = self.expect_end(Token::ParenClose);

        self.node_at(start, end, TupleExpression {
            expressions,
        })
    }

    fn prefix_expression(&mut self, operator: PrefixOperator) -> Option<ExpressionNode<'ast>> {
        let operator: Node<_> = self.node_at_token(operator);
        let operand = expect!(self, self.expression(P2));

        self.node_at(operator.start, operand.end, PrefixExpression {
            operator,
            operand,
        })
    }

    fn integer_number(&mut self) -> Option<ExpressionNode<'ast>> {
        let number = self.lexer.slice();
        let (start, end) = self.loc();

        self.lexer.advance();

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

        let end = self.end_then_advance();

        self.node_at(start, end, Primitive::IntegerNumber(number, unit))
    }
}


#[cfg(test)]
mod test {
    use super::*;
    use mock::{Mock, assert_units};

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
