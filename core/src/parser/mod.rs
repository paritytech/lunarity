#[cfg(test)] mod mock;
#[macro_use] mod expect_macro;

mod source;
mod type_name;
mod contract;
mod function;
mod expression;
mod nested;
mod statement;
mod assembly;

use toolshed::Arena;
use toolshed::list::GrowableList;

pub use parser::statement::{StatementContext, FunctionContext, ModifierContext};
pub use parser::nested::*;

use ast::*;
use lexer::{Lexer, Token};
use lexer::Token::*;
use error::Error;


pub struct Parser<'ast> {
    arena: &'ast Arena,

    /// Lexer will produce tokens from the source
    lexer: Lexer<'ast>,

    /// Errors occurred during parsing
    errors: Vec<Error>,

    /// AST under construction
    body: SourceUnitList<'ast>,
}

impl<'ast> Parser<'ast> {
    pub fn new(source: &str, arena: &'ast Arena) -> Self {
        Parser {
            arena,
            lexer: Lexer::new(arena, source),
            errors: Vec::new(),
            body: NodeList::empty(),
        }
    }

    #[inline]
    fn allow(&mut self, token: Token) -> bool {
        if self.lexer.token == token {
            self.lexer.consume();
            true
        } else {
            false
        }
    }

    #[inline]
    fn expect(&mut self, token: Token) {
        if self.lexer.token == token {
            self.lexer.consume();
        } else {
            self.error();
        }
    }

    #[inline]
    fn expect_exact(&mut self, token: Token, expected: &str) {
        if self.lexer.token == token && self.lexer.token_as_str() == expected {
            self.lexer.consume();
        } else {
            self.error();
        }
    }

    #[inline]
    fn expect_end(&mut self, token: Token) -> u32 {
        let end = self.lexer.end();

        self.expect(token);

        end
    }

    #[inline]
    fn str_node<R>(&mut self) -> R
    where
        R: From<Node<'ast, &'ast str>>,
    {
        let node = self.lexer.token_as_str();

        self.node_at_token(node)
    }

    #[inline]
    fn expect_str_node(&mut self, token: Token) -> Node<'ast, &'ast str> {
        let val          = self.lexer.token_as_str();
        let (start, end) = self.lexer.loc();

        self.expect(token);

        self.node_at(start, end, val)
    }

    #[inline]
    fn allow_str_node(&mut self, token: Token) -> Option<Node<'ast, &'ast str>> {
        if self.lexer.token == token {
            self.str_node()
        } else {
            None
        }
    }

    #[inline]
    fn allow_flag_node(&mut self, token: Token) -> Option<FlagNode<'ast>> {
        if self.lexer.token == token {
            self.node_at_token(Flag)
        } else {
            None
        }
    }

    fn error(&mut self) {
        self.errors.push(self.lexer.invalid_token());
    }

    #[inline]
    fn alloc<T>(&mut self, val: NodeInner<T>) -> Node<'ast, T>
    where
        T: Copy,
    {
        Node::new(self.arena.alloc(val))
    }

    #[inline]
    fn node_at<T, I, R>(&mut self, start: u32, end: u32, item: I) -> R
    where
        T: 'ast + Copy,
        I: Into<T>,
        R: From<Node<'ast, T>>,
    {
        From::from(self.alloc(NodeInner::new(start, end, item.into())))
    }

    #[inline]
    fn node_at_token<T, I, R>(&mut self, item: I) -> R
    where
        T: 'ast + Copy,
        I: Into<T>,
        R: From<Node<'ast, T>>,
    {
        let (start, end) = self.lexer.loc();

        self.lexer.consume();

        self.node_at(start, end, item)
    }

    #[inline]
    fn parse(&mut self) {
        let builder = GrowableList::new();

        while let Some(unit) = self.source_unit() {
            builder.push(self.arena, unit);
        }

        self.body = builder.as_list();

        self.expect(EndOfProgram);
    }

    #[inline]
    fn unique_flag<F>(&mut self, at: &mut Option<Node<'ast, F>>, flag: F)
    where
        F: Copy,
    {
        if at.is_some() {
            // TODO: More descriptive errors, something like "Can't redeclare visibility/mutability"
            return self.error();
        }

        *at = self.node_at_token(flag);
    }
}

/// Parse the Solidity source from `&str` and produce an Abstract Syntax Tree for it.
pub fn parse<'src, 'ast>(source: &'src str) -> Result<Program<'ast>, Vec<Error>> {
    let arena = Arena::new();

    let (body, errors) = {
        let mut parser = Parser::new(source, &arena);

        parser.parse();

        (parser.body.into_unsafe(), parser.errors)
    };

    match errors.len() {
        0 => Ok(Program::new(body, arena)),
        _ => Err(errors)
    }
}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn can_parse_second_price_auction() {
        let source = include_str!("../../benches/second-price-auction.sol");

        parse(source).unwrap();
    }
}
