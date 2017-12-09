use toolshed::list::ListBuilder;

use ast::*;
use parser::{Parser, Precedence, TopPrecedence};
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
            Token::Identifier      => return self.identifier_expression(),
            Token::ParenOpen       => return self.tuple_expression(),
            Token::LiteralTrue     => Primitive::Bool(true),
            Token::LiteralFalse    => Primitive::Bool(false),
            Token::LiteralHex      => Primitive::HexNumber(self.lexer.token_as_str()),
            Token::LiteralInteger  => Primitive::IntegerNumber(self.lexer.token_as_str()),
            Token::LiteralRational => Primitive::RationalNumber(self.lexer.token_as_str()),
            Token::LiteralString   => Primitive::String(self.lexer.token_as_str()),

            _ => return None,
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
}
