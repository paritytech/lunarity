use toolshed::list::{ListBuilder, GrowableList};

use ast::*;
use parser::Parser;
use lexer::Token;

impl<'ast> Parser<'ast> {
    pub fn contract_definition(&mut self) -> Option<SourceUnitNode<'ast>> {
        let start = self.lexer.start_then_consume();
        let name = self.expect_str_node(Token::Identifier);

        let inherits = if self.allow(Token::KeywordIs) {
            let builder = ListBuilder::new(self.arena, self.expect_str_node(Token::Identifier));

            while self.allow(Token::Comma) {
                builder.push(self.arena, self.expect_str_node(Token::Identifier));
            }

            builder.as_list()
        } else {
            NodeList::empty()
        };

        self.expect(Token::BraceOpen);

        let builder = GrowableList::new();

        while let Some(part) = self.contract_part() {
            builder.push(self.arena, part);
        }

        let end = self.expect_end(Token::BraceClose);

        Some(self.node_at(start, end, ContractDefinition {
            name,
            inherits,
            body: builder.as_list(),
        }))
    }

    fn contract_part(&mut self) -> Option<Node<'ast, ContractPart<'ast>>> {
        match self.lexer.token {
            Token::DeclarationEvent => {
                let start  = self.lexer.start_then_consume();
                let name   = self.expect_str_node(Token::Identifier);

                self.expect(Token::ParenOpen);

                let params = match self.indexed_parameter() {
                    Some(param) => {
                        let builder = ListBuilder::new(self.arena, param);

                        while self.allow(Token::Comma) {
                            match self.indexed_parameter() {
                                Some(param) => builder.push(self.arena, param),
                                None        => self.error(),
                            }
                        }

                        builder.as_list()
                    },
                    None => NodeList::empty(),
                };

                self.expect(Token::ParenClose);

                let anonymous = self.allow(Token::KeywordAnonymous);
                let end       = self.expect_end(Token::Semicolon);

                Some(self.node_at(start, end, EventDefinition {
                    anonymous,
                    name,
                    params,
                }))
            },
            _ => None,
        }
    }

    fn indexed_parameter(&mut self) -> Option<Node<'ast, IndexedParameter<'ast>>> {
        let type_name = self.type_name()?;
        let indexed   = self.allow(Token::KeywordIndexed);
        let name      = self.expect_str_node(Token::Identifier);

        Some(self.node_at(type_name.start, name.end, IndexedParameter {
            indexed,
            type_name,
            name,
        }))
    }

    // TODO: it's own file.
    fn type_name(&mut self) -> Option<TypeNameNode<'ast>> {
        let elementary = {
            let ref size = self.lexer.type_size;

            match self.lexer.token {
                Token::TypeBool       => ElementaryTypeName::Bool,
                Token::TypeAddress    => ElementaryTypeName::Address,
                Token::TypeString     => ElementaryTypeName::String,
                Token::DeclarationVar => ElementaryTypeName::Var,
                Token::TypeBytes      => ElementaryTypeName::Byte(size.0),
                Token::TypeInt        => ElementaryTypeName::Int(size.0),
                Token::TypeUint       => ElementaryTypeName::Uint(size.0),
                Token::TypeFixed      => ElementaryTypeName::Fixed(size.0, size.1),
                Token::TypeUfixed     => ElementaryTypeName::Ufixed(size.0, size.1),
                _                     => return None,
            }
        };

        let node = self.node_at_token(elementary);

        self.lexer.consume();

        Some(node)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use parser::mock::{Mock, assert_units};

    #[test]
    fn empty_contract() {
        let m = Mock::new();

        assert_units(r#"

            contract Foo {}
            contract Doge is Amazing {}
            contract This is Silly, Kinda {}

        "#, [
            m.node(14, 29, ContractDefinition {
                name: m.node(23, 26, "Foo"),
                inherits: NodeList::empty(),
                body: NodeList::empty(),
            }),
            m.node(42, 69, ContractDefinition {
                name: m.node(51, 55, "Doge"),
                inherits: m.list([
                    m.node(59, 66, "Amazing"),
                ]),
                body: NodeList::empty(),
            }),
            m.node(82, 114, ContractDefinition {
                name: m.node(91, 95, "This"),
                inherits: m.list([
                    m.node(99, 104, "Silly"),
                    m.node(106, 111, "Kinda"),
                ]),
                body: NodeList::empty(),
            }),
        ]);
    }

    #[test]
    fn empty_events() {
        let m = Mock::new();

        assert_units(r#"

            contract Foo {
                event Horizon();
                event Alcoholics() anonymous;
            }

        "#, [
            m.node(14, 121, ContractDefinition {
                name: m.node(23, 26, "Foo"),
                inherits: NodeList::empty(),
                body: m.list([
                    m.node(45, 61, EventDefinition {
                        anonymous: false,
                        name: m.node(51, 58, "Horizon"),
                        params: NodeList::empty(),
                    }),
                    m.node(78, 107, EventDefinition {
                        anonymous: true,
                        name: m.node(84, 94, "Alcoholics"),
                        params: NodeList::empty(),
                    }),
                ]),
            }),
        ]);
    }

    #[test]
    fn event_with_parameters() {
        let m = Mock::new();

        assert_units(r#"

            contract Foo {
                event Horizon(int32 indexed foo, bool bar);
            }

        "#, [
            m.node(14, 102, ContractDefinition {
                name: m.node(23, 26, "Foo"),
                inherits: NodeList::empty(),
                body: m.list([
                    m.node(45, 88, EventDefinition {
                        anonymous: false,
                        name: m.node(51, 58, "Horizon"),
                        params: m.list([
                            m.node(59, 76, IndexedParameter {
                                indexed: true,
                                type_name: m.node(59, 64, ElementaryTypeName::Int(4)),
                                name: m.node(73, 76, "foo"),
                            }),
                            m.node(78, 86, IndexedParameter {
                                indexed: false,
                                type_name: m.node(78, 82, ElementaryTypeName::Bool),
                                name: m.node(83, 86, "bar"),
                            }),
                        ]),
                    }),
                ]),
            }),
        ]);
    }
}
