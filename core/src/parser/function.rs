// use toolshed::list::{ListBuilder, GrowableList};

use ast::*;
use parser::Parser;
use lexer::Token;

impl<'ast> Parser<'ast> {
    pub fn function_definition(&mut self) -> Option<ContractPartNode<'ast>> {
        let start = self.lexer.start_then_consume();

        let name = match self.lexer.token {
            Token::Identifier => Some(self.str_node()),
            _                 => None,
        };

        self.expect(Token::ParenOpen);
        self.expect(Token::ParenClose);

        let mut mutability = None;
        let mut visibility = None;

        loop {
            match self.lexer.token {
                Token::KeywordExternal => self.unique_flag(FunctionVisibility::External, &mut visibility),
                Token::KeywordPublic   => self.unique_flag(FunctionVisibility::Public, &mut visibility),
                Token::KeywordInternal => self.unique_flag(FunctionVisibility::Internal, &mut visibility),
                Token::KeywordPrivate  => self.unique_flag(FunctionVisibility::Private, &mut visibility),

                Token::KeywordPure     => self.unique_flag(StateMutability::Pure, &mut mutability),
                Token::KeywordConstant => self.unique_flag(StateMutability::Constant, &mut mutability),
                Token::KeywordView     => self.unique_flag(StateMutability::View, &mut mutability),
                Token::KeywordPayable  => self.unique_flag(StateMutability::Payable, &mut mutability),

                _ => break,
            }
        }

        let end = self.expect_end(Token::Semicolon);

        Some(self.node_at(start, end, FunctionDefinition {
            name,
            params: NodeList::empty(),
            visibility,
            mutability,
            returns: NodeList::empty(),
            body: None,
        }))
    }

    #[inline]
    fn unique_flag<M>(&mut self, marker: M, at: &mut Option<M>) {
        if at.is_some() {
            return self.error();
        }

        *at = Some(marker);

        self.lexer.consume();
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use parser::mock::{Mock, assert_units};

    #[test]
    fn empty_function() {
        let m = Mock::new();

        assert_units(r#"

            contract Foo {
                function();
                function bar();
            }

        "#, [
            m.node(14, 102, ContractDefinition {
                name: m.node(23, 26, "Foo"),
                inherits: NodeList::empty(),
                body: m.list([
                    m.node(45, 56, FunctionDefinition {
                        name: None,
                        params: NodeList::empty(),
                        visibility: None,
                        mutability: None,
                        returns: NodeList::empty(),
                        body: None,
                    }),
                    m.node(73, 88, FunctionDefinition {
                        name: m.node(82, 85, "bar"),
                        params: NodeList::empty(),
                        visibility: None,
                        mutability: None,
                        returns: NodeList::empty(),
                        body: None,
                    }),
                ]),
            }),
        ]);
    }

    #[test]
    fn function_mutability_and_visibility() {
        let m = Mock::new();

        assert_units(r#"

            contract Foo {
                function wow() pure external;
                function such() internal view;
                function very() private;
                function much() payable;
            }

        "#, [
            m.node(14, 217, ContractDefinition {
                name: m.node(23, 26, "Foo"),
                inherits: NodeList::empty(),
                body: m.list([
                    m.node(45, 74, FunctionDefinition {
                        name: m.node(54, 57, "wow"),
                        params: NodeList::empty(),
                        visibility: Some(FunctionVisibility::External),
                        mutability: Some(StateMutability::Pure),
                        returns: NodeList::empty(),
                        body: None,
                    }),
                    m.node(91, 121, FunctionDefinition {
                        name: m.node(100, 104, "such"),
                        params: NodeList::empty(),
                        visibility: Some(FunctionVisibility::Internal),
                        mutability: Some(StateMutability::View),
                        returns: NodeList::empty(),
                        body: None,
                    }),
                    m.node(138, 162, FunctionDefinition {
                        name: m.node(147, 151, "very"),
                        params: NodeList::empty(),
                        visibility: Some(FunctionVisibility::Private),
                        mutability: None,
                        returns: NodeList::empty(),
                        body: None,
                    }),
                    m.node(179, 203, FunctionDefinition {
                        name: m.node(188, 192, "much"),
                        params: NodeList::empty(),
                        visibility: None,
                        mutability: Some(StateMutability::Payable),
                        returns: NodeList::empty(),
                        body: None,
                    }),
                ]),
            }),
        ]);
    }

    #[test]
    fn function_flags_are_unique_per_kind() {
        use parser::parse;

        assert!(parse("contract Foo { function() public public; }").is_err());
        assert!(parse("contract Foo { function() pure pure; }").is_err());
        assert!(parse("contract Foo { function() internal external; }").is_err());
        assert!(parse("contract Foo { function() payable constant; }").is_err());
    }
}
