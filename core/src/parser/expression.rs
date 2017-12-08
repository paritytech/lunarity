use toolshed::list::ListBuilder;

use ast::*;
use parser::Parser;
use lexer::Token;

impl<'ast> Parser<'ast> {
    pub fn expression(&mut self) -> Option<ExpressionNode<'ast>> {
        let primitive = match self.lexer.token {
            Token::ParenOpen       => return self.tuple_expression(),
            Token::LiteralTrue     => Primitive::Bool(true),
            Token::LiteralFalse    => Primitive::Bool(false),
            Token::LiteralHex      => Primitive::HexNumber(self.lexer.token_as_str()),
            Token::LiteralInteger  => Primitive::IntegerNumber(self.lexer.token_as_str()),
            Token::LiteralRational => Primitive::RationalNumber(self.lexer.token_as_str()),
            Token::LiteralString   => Primitive::String(self.lexer.token_as_str()),

            _ => return None,
        };

        Some(self.node_at_token_then_consume(primitive))
    }

    /// Same as `expression`, but produces an error on `None` variant. Safe to treat with `?`.
    #[inline]
    pub fn expect_expression(&mut self) -> Option<ExpressionNode<'ast>> {
        match self.expression() {
            None => {
                self.error();

                None
            },
            expression => expression,
        }
    }

    fn tuple_expression(&mut self) -> Option<ExpressionNode<'ast>> {
        let start       = self.lexer.start_then_consume();

        let expressions = match self.expression() {
            Some(expression) => {
                let builder = ListBuilder::new(self.arena, expression);

                while self.allow(Token::Comma) {
                    self.expect_expression()
                        .map(|expression| builder.push(self.arena, expression));
                }

                builder.as_list()
            },
            None => NodeList::empty(),
        };

        let end = self.expect_end(Token::ParenClose);

        Some(self.node_at(start, end, TupleExpression {
            expressions,
        }))
    }
}
