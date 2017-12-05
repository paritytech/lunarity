use parser::Parser;
use ast::*;
use lexer::Token::*;

impl<'ast> Parser<'ast> {
    #[inline]
    pub fn source_unit(&mut self) -> SourceUnitNode<'ast> {
        match self.lexer.token {
            KeywordPragma => {
                let start = self.lexer.start();

                self.lexer.advance();

                if self.lexer.token != Identifier || self.lexer.token_as_str() != "solidity" {
                    self.error();
                }

                let version = self.lexer.read_pragma();
                let end = self.lexer.end();

                self.expect(Semicolon);

                self.node_at(start, end, PragmaDirective {
                    version
                })
            },
            _ => panic!("Unimplemented, lexer at {}", self.lexer.token_as_str())
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use parse;

    fn assert_unit(source: &str, expected: NodeInner<SourceUnit>) {
        let program = parse("pragma solidity ^0.4.17;").unwrap();

        assert_eq!(**program.body().only_element().unwrap(), expected);
    }

    #[test]
    fn can_parse_pragma() {
        assert_unit("pragma solidity ^0.4.17;", NodeInner::new(0, 24, PragmaDirective {
            version: "^0.4.17"
        }));
    }
}
