use toolshed::list::{ListBuilder, GrowableList};

use ast::*;
use parser::Parser;
use lexer::Token;

impl<'ast> Parser<'ast> {
    pub fn function_definition(&mut self) -> Option<ContractPartNode<'ast>> {
        let start = self.lexer.start_then_consume();

        let name = match self.lexer.token {
            Token::Identifier => self.str_node(),
            _                 => None,
        };

        self.expect(Token::ParenOpen);

        let params = self.parameter_list();

        self.expect(Token::ParenClose);

        let mut mutability = None;
        let mut visibility = None;
        let modifiers = GrowableList::new();

        loop {
            match self.lexer.token {
                Token::KeywordExternal => self.unique_flag(&mut visibility, FunctionVisibility::External),
                Token::KeywordPublic   => self.unique_flag(&mut visibility, FunctionVisibility::Public),
                Token::KeywordInternal => self.unique_flag(&mut visibility, FunctionVisibility::Internal),
                Token::KeywordPrivate  => self.unique_flag(&mut visibility, FunctionVisibility::Private),

                Token::KeywordPure     => self.unique_flag(&mut mutability, StateMutability::Pure),
                Token::KeywordConstant => self.unique_flag(&mut mutability, StateMutability::Constant),
                Token::KeywordView     => self.unique_flag(&mut mutability, StateMutability::View),
                Token::KeywordPayable  => self.unique_flag(&mut mutability, StateMutability::Payable),

                _ => match self.modifier_invocation() {
                    Some(modifier) => modifiers.push(self.arena, modifier),
                    None           => break,
                }
            }
        }

        let modifiers = modifiers.as_list();
        let returns;

        if self.allow(Token::KeywordReturns) {
            self.expect(Token::ParenOpen);

            returns = self.parameter_list();

            self.expect(Token::ParenClose);
        } else {
            returns = NodeList::empty();
        }

        let end;

        let block = match self.lexer.token {
            Token::BraceOpen => {
                let block = self.block();

                end   = block.end;

                Some(block)
            },
            _ => {
                end   = self.expect_end(Token::Semicolon);

                None
            }
        };

        self.node_at(start, end, FunctionDefinition {
            name,
            params,
            visibility,
            mutability,
            modifiers,
            returns,
            block,
        })
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

    fn modifier_invocation(&mut self) -> Option<Node<'ast, ModifierInvocation<'ast>>> {
        let id = self.allow_str_node(Token::Identifier)?;

        let arguments;
        let end;

        if self.allow(Token::ParenOpen) {
            arguments = self.expression_list();
            end       = self.expect_end(Token::ParenClose);
        } else {
            arguments = NodeList::empty();
            end       = id.end;
        };

        self.node_at(id.start, end, ModifierInvocation {
            id,
            arguments,
        })
    }

    fn parameter_list(&mut self) -> ParameterList<'ast> {
        match self.parameter() {
            Some(param) => {
                let builder = ListBuilder::new(self.arena, param);

                while self.allow(Token::Comma) {
                    match self.parameter() {
                        Some(param) => builder.push(self.arena, param),
                        None        => self.error(),
                    }
                }

                builder.as_list()
            },
            None => NodeList::empty(),
        }
    }

    fn parameter(&mut self) -> Option<Node<'ast, Parameter<'ast>>> {
        let type_name = self.type_name()?;
        let name      = self.allow_str_node(Token::Identifier);

        let end = name.end().unwrap_or_else(|| type_name.end);

        self.node_at(type_name.start, end, Parameter {
            type_name,
            name,
        })
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
                        modifiers: NodeList::empty(),
                        returns: NodeList::empty(),
                        block: None,
                    }),
                    m.node(73, 88, FunctionDefinition {
                        name: m.node(82, 85, "bar"),
                        params: NodeList::empty(),
                        visibility: None,
                        mutability: None,
                        modifiers: NodeList::empty(),
                        returns: NodeList::empty(),
                        block: None,
                    }),
                ]),
            }),
        ]);
    }

    #[test]
    fn function_parameters() {
        let m = Mock::new();

        assert_units(r#"

            contract Foo {
                function(uint56, bool);
            }

        "#, [
            m.node(14, 82, ContractDefinition {
                name: m.node(23, 26, "Foo"),
                inherits: NodeList::empty(),
                body: m.list([
                    m.node(45, 68, FunctionDefinition {
                        name: None,
                        params: m.list([
                            m.node(54, 60, Parameter {
                                type_name: m.node(54, 60, ElementaryTypeName::Uint(7)),
                                name: None,
                            }),
                            m.node(62, 66, Parameter {
                                type_name: m.node(62, 66, ElementaryTypeName::Bool),
                                name: None,
                            }),
                        ]),
                        visibility: None,
                        mutability: None,
                        modifiers: NodeList::empty(),
                        returns: NodeList::empty(),
                        block: None,
                    }),
                ]),
            }),
        ]);
    }

    #[test]
    fn function_named_parameters() {
        let m = Mock::new();

        assert_units(r#"

            contract Foo {
                function doge(uint56 wow, bool moon);
            }

        "#, [
            m.node(14, 96, ContractDefinition {
                name: m.node(23, 26, "Foo"),
                inherits: NodeList::empty(),
                body: m.list([
                    m.node(45, 82, FunctionDefinition {
                        name: m.node(54, 58, "doge"),
                        params: m.list([
                            m.node(59, 69, Parameter {
                                type_name: m.node(59, 65, ElementaryTypeName::Uint(7)),
                                name: m.node(66, 69, "wow"),
                            }),
                            m.node(71, 80, Parameter {
                                type_name: m.node(71, 75, ElementaryTypeName::Bool),
                                name: m.node(76, 80, "moon"),
                            }),
                        ]),
                        visibility: None,
                        mutability: None,
                        modifiers: NodeList::empty(),
                        returns: NodeList::empty(),
                        block: None,
                    }),
                ]),
            }),
        ]);
    }

    #[test]
    fn function_returns() {
        let m = Mock::new();

        assert_units(r#"

            contract Foo {
                function doge() returns (uint56, bool);
            }

        "#, [
            m.node(14, 98, ContractDefinition {
                name: m.node(23, 26, "Foo"),
                inherits: NodeList::empty(),
                body: m.list([
                    m.node(45, 84, FunctionDefinition {
                        name: m.node(54, 58, "doge"),
                        params: NodeList::empty(),
                        visibility: None,
                        mutability: None,
                        modifiers: NodeList::empty(),
                        returns: m.list([
                            m.node(70, 76, Parameter {
                                type_name: m.node(70, 76, ElementaryTypeName::Uint(7)),
                                name: None,
                            }),
                            m.node(78, 82, Parameter {
                                type_name: m.node(78, 82, ElementaryTypeName::Bool),
                                name: None,
                            }),
                        ]),
                        block: None,
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
                        visibility: m.node(65, 73, FunctionVisibility::External),
                        mutability: m.node(60, 64, StateMutability::Pure),
                        modifiers: NodeList::empty(),
                        returns: NodeList::empty(),
                        block: None,
                    }),
                    m.node(91, 121, FunctionDefinition {
                        name: m.node(100, 104, "such"),
                        params: NodeList::empty(),
                        visibility: m.node(107, 115, FunctionVisibility::Internal),
                        mutability: m.node(116, 120, StateMutability::View),
                        modifiers: NodeList::empty(),
                        returns: NodeList::empty(),
                        block: None,
                    }),
                    m.node(138, 162, FunctionDefinition {
                        name: m.node(147, 151, "very"),
                        params: NodeList::empty(),
                        visibility: m.node(154, 161, FunctionVisibility::Private),
                        mutability: None,
                        modifiers: NodeList::empty(),
                        returns: NodeList::empty(),
                        block: None,
                    }),
                    m.node(179, 203, FunctionDefinition {
                        name: m.node(188, 192, "much"),
                        params: NodeList::empty(),
                        visibility: None,
                        mutability: m.node(195, 202, StateMutability::Payable),
                        modifiers: NodeList::empty(),
                        returns: NodeList::empty(),
                        block: None,
                    }),
                ]),
            }),
        ]);
    }

    #[test]
    fn function_modifiers() {
        let m = Mock::new();

        assert_units(r#"

            contract Foo {
                function() only_doges such pure moon;
            }

        "#, [
            m.node(14, 96, ContractDefinition {
                name: m.node(23, 26, "Foo"),
                inherits: NodeList::empty(),
                body: m.list([
                    m.node(45, 82, FunctionDefinition {
                        name: None,
                        params: NodeList::empty(),
                        visibility: None,
                        mutability: m.node(72, 76, StateMutability::Pure),
                        modifiers: m.list([
                            m.node(56, 66, ModifierInvocation {
                                id: m.node(56, 66, "only_doges"),
                                arguments: NodeList::empty(),
                            }),
                            m.node(67, 71, ModifierInvocation {
                                id: m.node(67, 71, "such"),
                                arguments: NodeList::empty(),
                            }),
                            m.node(77, 81, ModifierInvocation {
                                id: m.node(77, 81, "moon"),
                                arguments: NodeList::empty(),
                            }),
                        ]),
                        returns: NodeList::empty(),
                        block: None,
                    }),
                ]),
            }),
        ]);
    }

    #[test]
    fn function_flags_are_unique_per_kind() {
        use parser::parse;

        // TODO: Better errors
        assert!(parse("contract Foo { function() public public; }").is_err());
        assert!(parse("contract Foo { function() pure pure; }").is_err());
        assert!(parse("contract Foo { function() internal external; }").is_err());
        assert!(parse("contract Foo { function() payable constant; }").is_err());
    }
}
