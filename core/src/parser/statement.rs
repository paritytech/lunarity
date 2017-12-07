use toolshed::list::GrowableList;

use ast::*;
use parser::Parser;
use lexer::Token;

impl<'ast> Parser<'ast> {
    pub fn statement(&mut self) -> Option<StatementNode<'ast>> {
        None
    }

    pub fn block<B>(&mut self) -> Node<'ast, B>
    where
        B: From<BlockStatement<'ast>> + Copy,
    {
        let start = self.lexer.start_then_consume();

        let body = GrowableList::new();

        while let Some(statement) = self.statement() {
            body.push(self.arena, statement);
        }

        let end = self.expect_end(Token::BraceClose);

        self.node_at(start, end, BlockStatement {
            body: body.as_list(),
        })
    }
}


#[cfg(test)]
mod test {
    use super::*;
    use parser::mock::{Mock, assert_units};

    #[test]
    fn function_mutability_and_visibility() {
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
                        block: m.node::<BlockStatement, BlockStatement, Option<Node<BlockStatement>>>(60, 62, BlockStatement {
                            body: NodeList::empty(),
                        })
                    }),
                ]),
            }),
        ]);
    }
}
