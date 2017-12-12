use toolshed::list::ListBuilder;

use ast::*;
use parser::{Parser, Precedence, Precedence2, TopPrecedence};
use lexer::Token;

impl<'ast> Parser<'ast> {
    #[inline]
    pub fn expression<P>(&mut self) -> Option<ExpressionNode<'ast>>
    where
        P: Precedence
    {
        self.bound_expression()
            .map(|expression| self.nested_expression::<P>(expression))
    }

    pub fn bound_expression(&mut self) -> Option<ExpressionNode<'ast>> {
        let primitive = match self.lexer.token {
            Token::KeywordThis         => return self.node_at_token(ThisExpression),
            Token::Identifier          => return self.identifier_expression(),
            Token::IdentifierBuiltin   => return self.identifier_expression(),
            Token::ParenOpen           => return self.tuple_expression(),
            Token::OperatorLogicalNot  => return self.prefix_expression(PrefixOperator::LogicalNot),
            Token::OperatorBitNot      => return self.prefix_expression(PrefixOperator::BitNot),
            Token::KeywordDelete       => return self.prefix_expression(PrefixOperator::Delete),
            Token::OperatorIncrement   => return self.prefix_expression(PrefixOperator::Increment),
            Token::OperatorDecrement   => return self.prefix_expression(PrefixOperator::Decrement),
            Token::OperatorAddition    => return self.prefix_expression(PrefixOperator::Plus),
            Token::OperatorSubtraction => return self.prefix_expression(PrefixOperator::Minus),
            Token::LiteralTrue         => Primitive::Bool(true),
            Token::LiteralFalse        => Primitive::Bool(false),
            Token::LiteralHex          => Primitive::HexNumber(self.lexer.token_as_str()),
            Token::LiteralInteger      => return self.integer_number(),
            Token::LiteralRational     => Primitive::RationalNumber(self.lexer.token_as_str()),
            Token::LiteralString       => Primitive::String(self.lexer.token_as_str()),

            _ => return self.elementary_type_name(),
        };

        self.node_at_token(primitive)
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
