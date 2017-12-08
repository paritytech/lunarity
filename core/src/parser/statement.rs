use toolshed::list::{List, GrowableList, ListBuilder};

use ast::*;
use parser::Parser;
use lexer::Token;

impl<'ast> Parser<'ast> {
    pub fn statement(&mut self) -> Option<StatementNode<'ast>> {
        match self.lexer.token {
            Token::DeclarationVar  => self.inferred_definition_statement(),
            _                      => self.variable_definition_statement(),
        }
    }

    pub fn block<B>(&mut self) -> Node<'ast, B>
    where
        B: From<Block<'ast>> + Copy,
    {
        let start = self.lexer.start_then_consume();
        let body  = GrowableList::new();

        while let Some(statement) = self.statement() {
            body.push(self.arena, statement);
        }

        let end = self.expect_end(Token::BraceClose);

        self.node_at(start, end, Block {
            body: body.as_list(),
        })
    }

    fn variable_definition_statement(&mut self) -> Option<StatementNode<'ast>> {
        let declaration = self.variable_declaration()?;

        let init;

        if self.allow(Token::Assign) {
            init = self.expression();

            if init.is_none() {
                self.error();
            }
        } else {
            init = None
        }

        let end = self.expect_end(Token::Semicolon);

        Some(self.node_at(declaration.start, end, VariableDefinitionStatement {
            declaration,
            init,
        }))
    }

    fn inferred_definition_statement(&mut self) -> Option<StatementNode<'ast>> {
        let start = self.lexer.start_then_consume();

        let ids = if self.allow(Token::ParenOpen) {
            self.tuple_destructing()
        } else {
            List::from(self.arena, Some(self.expect_str_node(Token::Identifier)))
        };

        self.expect(Token::Assign);

        let init = self.expect_expression()?;
        let end  = self.expect_end(Token::Semicolon);

        Some(self.node_at(start, end, InferredDefinitionStatement {
            ids,
            init,
        }))
    }

    fn tuple_destructing(&mut self) -> List<'ast, Option<IdentifierNode<'ast>>> {
        if self.allow(Token::ParenClose) {
            return List::empty();
        }

        let builder = ListBuilder::new(self.arena, self.allow_str_node(Token::Identifier));

        while self.allow(Token::Comma) {
            builder.push(self.arena, self.allow_str_node(Token::Identifier));
        }

        self.expect(Token::ParenClose);

        builder.as_list()
    }
}


#[cfg(test)]
mod test {
    use super::*;
    use parser::mock::{Mock, assert_units};

    #[test]
    fn empty_block() {
        let m = Mock::new();

        assert_units(r#"

            contract Foo {
                function wow() {}
            }

        "#, [
            m.node(14, 76, ContractDefinition {
                name: m.node(23, 26, "Foo"),
                inherits: NodeList::empty(),
                body: m.list([
                    m.node(45, 62, FunctionDefinition {
                        name: m.node(54, 57, "wow"),
                        params: NodeList::empty(),
                        visibility: None,
                        mutability: None,
                        modifiers: NodeList::empty(),
                        returns: NodeList::empty(),
                        block: m.node(60, 62, Block {
                            body: NodeList::empty(),
                        })
                    }),
                ]),
            }),
        ]);
    }

    #[test]
    fn variable_definition_statement() {
        let m = Mock::new();

        assert_units(r#"

            contract Foo {
                function wow() {
                    uint foo = 10;
                    bool memory bar = true;
                    string storage baz;
                }
            }

        "#, [
            m.node(14, 212, ContractDefinition {
                name: m.node(23, 26, "Foo"),
                inherits: NodeList::empty(),
                body: m.list([
                    m.node(45, 198, FunctionDefinition {
                        name: m.node(54, 57, "wow"),
                        params: NodeList::empty(),
                        visibility: None,
                        mutability: None,
                        modifiers: NodeList::empty(),
                        returns: NodeList::empty(),
                        block: m.node(60, 198, Block {
                            body: m.list([
                                m.node(82, 96, VariableDefinitionStatement {
                                    declaration: m.node(82, 90, VariableDeclaration {
                                        type_name: m.node(82, 86, ElementaryTypeName::Uint(32)),
                                        location: None,
                                        id: m.node(87, 90, "foo"),
                                    }),
                                    init: m.node(93, 95, Primitive::IntegerNumber("10")),
                                }),
                                m.node(117, 140, VariableDefinitionStatement {
                                    declaration: m.node(117, 132, VariableDeclaration {
                                        type_name: m.node(117, 121, ElementaryTypeName::Bool),
                                        location: m.node(122, 128, StorageLocation::Memory),
                                        id: m.node(129, 132, "bar"),
                                    }),
                                    init: m.node(135, 139, Primitive::Bool(true)),
                                }),
                                m.node(161, 180, VariableDefinitionStatement {
                                    declaration: m.node(161, 179, VariableDeclaration {
                                        type_name: m.node(161, 167, ElementaryTypeName::String),
                                        location: m.node(168, 175, StorageLocation::Storage),
                                        id: m.node(176, 179, "baz"),
                                    }),
                                    init: None,
                                }),
                            ]),
                        }),
                    }),
                ]),
            }),
        ]);
    }

    #[test]
    fn inferred_definition_statement() {
        let m = Mock::new();

        assert_units(r#"

            contract Foo {
                function wow() {
                    var foo = 10;
                    var (a, b, c) = (1, 2, 3);
                    var () = ();
                    var (,,skip) = (4, 5, 6);
                }
            }

        "#, [
            m.node(14, 253, ContractDefinition {
                name: m.node(23, 26, "Foo"),
                inherits: NodeList::empty(),
                body: m.list([
                    m.node(45, 239, FunctionDefinition {
                        name: m.node(54, 57, "wow"),
                        params: NodeList::empty(),
                        visibility: None,
                        mutability: None,
                        modifiers: NodeList::empty(),
                        returns: NodeList::empty(),
                        block: m.node(60, 239, Block {
                            body: m.list([
                                m.node(82, 95, InferredDefinitionStatement {
                                    ids: m.list([ m.node(86, 89, "foo") ]),
                                    init: m.node(92, 94, Primitive::IntegerNumber("10")),
                                }),
                                m.node(116, 142, InferredDefinitionStatement {
                                    ids: m.list([
                                        m.node(121, 122, "a"),
                                        m.node(124, 125, "b"),
                                        m.node(127, 128, "c"),
                                    ]),
                                    init: m.node(132, 141, TupleExpression {
                                        expressions: m.list([
                                            m.node(133, 134, Primitive::IntegerNumber("1")),
                                            m.node(136, 137, Primitive::IntegerNumber("2")),
                                            m.node(139, 140, Primitive::IntegerNumber("3")),
                                        ])
                                    }),
                                }),
                                m.node(163, 175, InferredDefinitionStatement {
                                    ids: List::empty(),
                                    init: m.node(172, 174, TupleExpression {
                                        expressions: NodeList::empty(),
                                    }),
                                }),
                                m.node(196, 221, InferredDefinitionStatement {
                                    ids: m.list([
                                        None,
                                        None,
                                        m.node(203, 207, "skip"),
                                    ]),
                                    init: m.node(211, 220, TupleExpression {
                                        expressions: m.list([
                                            m.node(212, 213, Primitive::IntegerNumber("4")),
                                            m.node(215, 216, Primitive::IntegerNumber("5")),
                                            m.node(218, 219, Primitive::IntegerNumber("6")),
                                        ])
                                    }),
                                }),
                            ]),
                        }),
                    }),
                ]),
            }),
        ]);
    }
}
