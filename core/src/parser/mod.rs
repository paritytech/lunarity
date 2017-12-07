#[cfg(test)]
mod mock;
mod source;
mod type_name;
mod contract;
mod function;

use toolshed::Arena;
use toolshed::list::GrowableList;

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

        if self.lexer.token == token {
            self.lexer.consume();
        } else {
            self.error();
        }

        end
    }

    #[inline]
    fn str_node(&mut self) -> Node<'ast, &'ast str> {
        let node = self.lexer.token_as_str();

        self.node_at_token_then_consume(node)
    }

    #[inline]
    fn expect_str_node(&mut self, token: Token) -> Node<'ast, &'ast str> {
        let node = self.lexer.token_as_str();
        let node = self.node_at_token(node);

        self.expect(token);

        node
    }

    #[inline]
    fn allow_str_node(&mut self, token: Token) -> Option<Node<'ast, &'ast str>> {
        if self.lexer.token == token {
            Some(self.str_node())
        } else {
            None
        }
    }

    #[inline]
    fn allow_flag_node(&mut self, token: Token) -> Option<FlagNode<'ast>> {
        if self.lexer.token == token {
            Some(self.node_at_token_then_consume(Flag))
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
    fn node_at<T, I>(&mut self, start: u32, end: u32, item: I) -> Node<'ast, T>
    where
        T: Copy,
        I: Into<T>,
    {
        self.alloc(NodeInner::new(start, end, item.into()))
    }

    #[inline]
    fn node_at_token<T, I>(&mut self, item: I) -> Node<'ast, T>
    where
        T: Copy,
        I: Into<T>,
    {
        let (start, end) = self.lexer.loc();

        self.node_at(start, end, item)
    }

    #[inline]
    fn node_at_token_then_consume<T, I>(&mut self, item: I) -> Node<'ast, T>
    where
        T: Copy,
        I: Into<T>,
    {
        let node = self.node_at_token(item);

        self.lexer.consume();

        node
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

    // #[inline]
    // fn block<I>(&mut self) -> BlockNode<'ast, I> where
    //     I: Parse<'ast, Output = Node<'ast, I>> + Copy
    // {
    //     let start = self.lexer.start();

    //     match self.lexer.token {
    //         BraceOpen => self.lexer.consume(),
    //         _         => self.error::<()>(),
    //     }

    //     let block = self.raw_block();
    //     let end   = self.lexer.end_then_consume();

    //     self.alloc_at_loc(start, end, block)
    // }

    // /// Same as above, but assumes that the opening brace has already been checked
    // #[inline]
    // fn unchecked_block<I>(&mut self) -> BlockNode<'ast, I> where
    //     I: Parse<'ast, Output = Node<'ast, I>> + Copy
    // {
    //     let start = self.lexer.start_then_consume();
    //     let block = self.raw_block();
    //     let end   = self.lexer.end_then_consume();

    //     self.alloc_at_loc(start, end, block)
    // }

    // #[inline]
    // fn raw_block<I>(&mut self) -> Block<'ast, I> where
    //     I: Parse<'ast, Output = Node<'ast, I>> + Copy
    // {
    //     if self.lexer.token == BraceClose {
    //         return Block { body: NodeList::empty() };
    //     }

    //     let statement = I::parse(self);
    //     let builder = ListBuilder::new(self.arena, statement);

    //     while self.lexer.token != BraceClose && self.lexer.token != EndOfProgram {
    //         builder.push(self.arena, I::parse(self));
    //     }

    //     Block { body: builder.as_list() }
    // }

    // #[inline]
    // fn identifier(&mut self) -> IdentifierNode<'ast> {
    //     match self.lexer.token {
    //         Identifier => {
    //             let ident = self.lexer.token_as_str();
    //             let ident = self.alloc_in_loc(ident);
    //             self.lexer.consume();
    //             ident
    //         },
    //         _ => self.error()
    //     }
    // }
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
