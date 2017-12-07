use ast::*;
use parser::Parser;
use lexer::Token;

impl<'ast> Parser<'ast> {
    pub fn expression(&mut self) -> Option<ExpressionNode<'ast>> {
        None
    }
}
