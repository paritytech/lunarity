use toolshed::list::GrowableList;

use ast::*;
use Parser;
use lexer::Token;

impl<'ast> Parser<'ast> {
    pub fn inline_assembly_block<B>(&mut self) -> Option<Node<'ast, B>>
    where
        B: From<InlineAssemblyBlock<'ast>> + Copy,
    {
        let start = self.start_then_advance();
        let items = GrowableList::new();

        while let Some(item) = self.assembly_item() {
            items.push(self.arena, item);
        }

        let end = self.expect_end(Token::BraceClose);

        self.node_at(start, end, InlineAssemblyBlock {
            items: items.as_list()
        })
    }

    fn assembly_item(&mut self) -> Option<AssemblyItemNode<'ast>> {
        match self.lexer.token {
            Token::BraceOpen  => self.inline_assembly_block(),
            Token::Identifier => self.assembly_identifier(),
            _                 => None,
        }
    }

    fn assembly_identifier(&mut self) -> Option<AssemblyItemNode<'ast>> {
        let (start, end) = self.loc();
        let identifier = self.lexer.slice();

        self.lexer.advance();

        if self.allow(Token::AssemblyBind) {
            let id   = self.node_at(start, end, identifier);
            let init = expect!(self, self.functional_assembly_expression());

            return self.node_at(start, init.end, AssemblyAssignment {
                id,
                init,
            });
        }

        self.node_at(start, end, identifier)
    }

    fn functional_assembly_expression(&mut self) -> Option<FunctionalAssemblyExpressionNode<'ast>> {
        let id = self.expect_str_node(Token::Identifier);

        self.expect(Token::ParenOpen);

        let arguments = GrowableList::new();

        while let Some(item) = self.assembly_item() {
            arguments.push(self.arena, item);
        }

        let end = self.expect_end(Token::ParenClose);

        self.node_at(id.start, end, FunctionalAssemblyExpression {
            id,
            arguments: arguments.as_list()
        })
    }
}
