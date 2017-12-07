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

        let end = self.expect_end(Token::Semicolon);

        Some(self.node_at(start, end, FunctionDefinition {
            name,
            params: NodeList::empty(),
            visibility: None,
            state: None,
            returns: NodeList::empty(),
            body: None,
        }))
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
                        state: None,
                        returns: NodeList::empty(),
                        body: None,
                    }),
                    m.node(73, 88, FunctionDefinition {
                        name: m.node(82, 85, "bar"),
                        params: NodeList::empty(),
                        visibility: None,
                        state: None,
                        returns: NodeList::empty(),
                        body: None,
                    }),
                ]),
            }),
        ]);
    }
}
