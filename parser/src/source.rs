use toolshed::list::ListBuilder;

use ast::*;
use Parser;
use lexer::Token;

impl<'ast> Parser<'ast> {
    pub fn source_unit(&mut self) -> Option<SourceUnitNode<'ast>> {
        match self.lexer.token {
            Token::KeywordPragma => self.pragma_directive(),
            Token::KeywordImport => self.import_directive(),
            Token::DeclarationContract => self.contract_definition(),
            _ => None,
        }
    }

    fn pragma_directive(&mut self) -> Option<SourceUnitNode<'ast>> {
        let start = self.start_then_advance();

        if self.lexer.token != Token::Identifier || self.lexer.slice() != "solidity" {
            self.error();
        }

        let version = ::lexer::read_pragma(&mut self.lexer);
        let end     = self.expect_end(Token::Semicolon);

        self.node_at(start, end, PragmaDirective {
            version
        })
    }

    fn import_directive(&mut self) -> Option<SourceUnitNode<'ast>> {
        let start = self.start_then_advance();

        let symbol = match self.lexer.token {
            Token::OperatorMultiplication => {
                self.lexer.advance();

                None
            },
            Token::Identifier    => self.str_node(),
            Token::LiteralString => return self.import_directive_from(start),
            Token::BraceOpen     => return self.import_directive_from_many(start),
            _                    => return None,
        };

        let alias = self.allow_alias();

        self.expect_exact(Token::Identifier, "from");

        let source = self.expect_str_node(Token::LiteralString);
        let end    = self.expect_end(Token::Semicolon);

        self.node_at(start, end, ImportDirective::From {
            symbol,
            alias,
            source,
        })
    }

    fn import_directive_from(&mut self, start: u32) -> Option<SourceUnitNode<'ast>> {
        let source = self.str_node();
        let alias  = self.allow_alias();
        let end    = self.expect_end(Token::Semicolon);

        self.node_at(start, end, ImportDirective::Global {
            source,
            alias,
        })
    }

    fn import_directive_from_many(&mut self, start: u32) -> Option<SourceUnitNode<'ast>> {
        self.lexer.advance();

        let imports = ListBuilder::new(self.arena, self.import_node());

        while self.allow(Token::Comma) {
            imports.push(self.arena, self.import_node());
        }

        self.expect(Token::BraceClose);
        self.expect_exact(Token::Identifier, "from");

        let source = self.expect_str_node(Token::LiteralString);
        let end    = self.expect_end(Token::Semicolon);

        self.node_at(start, end, ImportDirective::ManyFrom {
            imports: imports.as_list(),
            source,
        })
    }

    fn import_node(&mut self) -> Node<'ast, Import<'ast>> {
        let symbol = self.expect_str_node(Token::Identifier);
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

    fn allow_alias(&mut self) -> Option<IdentifierNode<'ast>> {
        if self.allow(Token::KeywordAs) {
            Some(self.expect_str_node(Token::Identifier))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use mock::{Mock, assert_units};

    #[test]
    fn pragma() {
        let m = Mock::new();

        assert_units("pragma solidity ^0.4.17;", [
            m.node(0, 24, PragmaDirective {
                version: "solidity ^0.4.17"
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
