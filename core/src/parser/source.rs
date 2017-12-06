use toolshed::list::ListBuilder;
use parser::Parser;
use ast::*;
use lexer::Token::*;

impl<'ast> Parser<'ast> {
    #[inline]
    pub fn source_unit(&mut self) -> SourceUnitNode<'ast> {
        match self.lexer.token {
            KeywordPragma => self.pragma_directive(),
            KeywordImport => self.import_directive(),
            _ => panic!("Unimplemented, lexer at {:?} {:?}", self.lexer.token, self.lexer.loc())
        }
    }

    fn pragma_directive(&mut self) -> SourceUnitNode<'ast> {
        let start = self.lexer.start_then_consume();

        if self.lexer.token != Identifier || self.lexer.token_as_str() != "solidity" {
            self.error();
        }

        let version = self.lexer.read_pragma();
        let end     = self.expect_end(Semicolon);

        self.node_at(start, end, PragmaDirective {
            version
        })
    }

    fn import_directive(&mut self) -> SourceUnitNode<'ast> {
        let start = self.lexer.start_then_consume();

        let symbol = match self.lexer.token {
            OperatorMultiplication => {
                self.lexer.consume();

                None
            },
            Identifier    => Some(self.str_node()),
            LiteralString => return self.import_directive_from(start),
            BraceOpen     => return self.import_directive_from_many(start),
            _             => return self.invalid_import_directive(start),
        };

        let alias = self.allow_alias();

        self.expect_exact(Identifier, "from");

        let source = self.expect_str_node(LiteralString);
        let end    = self.expect_end(Semicolon);

        self.node_at(start, end, ImportDirective::From {
            symbol,
            alias,
            source,
        })
    }

    fn import_directive_from(&mut self, start: u32) -> SourceUnitNode<'ast> {
        let source = self.str_node();
        let alias  = self.allow_alias();
        let end    = self.expect_end(Semicolon);

        self.node_at(start, end, ImportDirective::Global {
            source,
            alias,
        })
    }

    fn import_directive_from_many(&mut self, start: u32) -> SourceUnitNode<'ast> {
        self.lexer.consume();

        let builder = ListBuilder::new(self.arena, self.import_node());

        loop {
            match self.lexer.token {
                Comma => {
                    self.lexer.consume();

                    builder.push(self.arena, self.import_node());
                },
                BraceClose => break self.lexer.consume(),
                _          => break self.error(),
            }
        }

        let imports = builder.as_list();

        self.expect_exact(Identifier, "from");

        let source = self.expect_str_node(LiteralString);
        let end    = self.expect_end(Semicolon);

        self.node_at(start, end, ImportDirective::ManyFrom {
            imports,
            source,
        })
    }

    fn import_node(&mut self) -> Node<'ast, Import<'ast>> {
        debug_assert_eq!(self.lexer.token, Identifier);

        let symbol = self.expect_str_node(Identifier);
        let alias = self.allow_alias();

        let end = match alias {
            Some(ref alias) => alias.end,
            None            => symbol.end,
        };

        self.node_at(symbol.start, end, Import {
            symbol,
            alias,
        })
    }

    fn allow_alias(&mut self) -> Option<Node<'ast, &'ast str>> {
        match self.lexer.token {
            KeywordAs => {
                self.lexer.consume();

                Some(self.expect_str_node(Identifier))
            },
            _ => None
        }
    }

    fn invalid_import_directive(&mut self, start: u32) -> SourceUnitNode<'ast> {
        let end = self.lexer.end();

        let source = self.lexer.token_as_str();
        let source = self.node_at_token(source);

        self.error();

        self.node_at(start, end, ImportDirective::Global {
            source,
            alias: None
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use parse;
    use parser::mock::Mock;

    fn assert_units<'mock, E>(source: &str, expected: E)
    where
        E: AsRef<[SourceUnitNode<'mock>]>
    {
        let program = parse(source).unwrap();

        let iter = program
                    .body()
                    .iter()
                    .zip(expected.as_ref().iter());

        for (got, expected) in iter {
            assert_eq!(got, expected);
        }

        assert_eq!(program.body().iter().count(), expected.as_ref().iter().count());
    }

    #[test]
    fn pragma() {
        let m = Mock::new();

        assert_units("pragma solidity ^0.4.17;", [
            m.node(0, 24, PragmaDirective {
                version: "^0.4.17"
            })
        ]);
    }

    #[test]
    fn import() {
        let m = Mock::new();

        assert_units(r#"

            import "foo";
            import * from "bar";
            import doge from "moon";
            import { doge, to, the } from "moon";

            "#, [
            m.node(14, 27, ImportDirective::Global {
                source: m.node(21, 26, "\"foo\""),
                alias: None,
            }),
            m.node(40, 60, ImportDirective::From {
                symbol: None,
                alias: None,
                source: m.node(54, 59, "\"bar\""),
            }),
            m.node(73, 97, ImportDirective::From {
                symbol: m.node(80, 84, "doge"),
                alias: None,
                source: m.node(90, 96, "\"moon\""),
            }),
            m.node(110, 147, ImportDirective::ManyFrom {
                imports: m.list([
                    m.node(119, 123, Import {
                        symbol: m.node(119, 123, "doge"),
                        alias: None,
                    }),
                    m.node(125, 127, Import {
                        symbol: m.node(125, 127, "to"),
                        alias: None,
                    }),
                    m.node(129, 132, Import {
                        symbol: m.node(129, 132, "the"),
                        alias: None,
                    }),
                ]),
                source: m.node(140, 146, "\"moon\""),
            })
        ]);
    }

    #[test]
    fn import_aliases() {
        let m = Mock::new();

        assert_units(r#"

            import "foo" as globalFoo;
            import * as globalBar from "bar";
            import doge as wow from "moon";
            import { doge as wow, to as such, the as parser } from "moon";

            "#, [
            m.node(14, 40, ImportDirective::Global {
                source: m.node(21, 26, "\"foo\""),
                alias: m.node(30, 39, "globalFoo"),
            }),
            m.node(53, 86, ImportDirective::From {
                symbol: None,
                alias: m.node(65, 74, "globalBar"),
                source: m.node(80, 85, "\"bar\""),
            }),
            m.node(99, 130, ImportDirective::From {
                symbol: m.node(106, 110, "doge"),
                alias: m.node(114, 117, "wow"),
                source: m.node(123, 129, "\"moon\""),
            }),
            m.node(143, 205, ImportDirective::ManyFrom {
                imports: m.list([
                    m.node(152, 163, Import {
                        symbol: m.node(152, 156, "doge"),
                        alias: m.node(160, 163, "wow"),
                    }),
                    m.node(165, 175, Import {
                        symbol: m.node(165, 167, "to"),
                        alias: m.node(171, 175, "such"),
                    }),
                    m.node(177, 190, Import {
                        symbol: m.node(177, 180, "the"),
                        alias: m.node(184, 190, "parser"),
                    }),
                ]),
                source: m.node(198, 204, "\"moon\""),
            })
        ]);
    }
}
