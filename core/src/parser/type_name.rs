use ast::*;
use parser::Parser;
use lexer::Token;

impl<'ast> Parser<'ast> {
    pub fn type_name(&mut self) -> Option<TypeNameNode<'ast>> {
        match self.lexer.token {
            Token::KeywordMapping => self.mapping(),
            Token::Identifier     => self.user_defined_type(),
            _                     => self.elementary_type_name(),
        }
    }

    pub fn type_name_in_statement(&mut self) -> Option<TypeNameNode<'ast>> {
        match self.lexer.token {
            Token::KeywordMapping => self.mapping(),
            _                     => self.elementary_type_name(),
        }
    }

    pub fn elementary_type_name<E>(&mut self) -> Option<Node<'ast, E>>
    where
        E: From<ElementaryTypeName> + Copy,
    {
        let elementary = {
            let ref size = self.lexer.type_size;

            match self.lexer.token {
                Token::TypeBool       => ElementaryTypeName::Bool,
                Token::TypeAddress    => ElementaryTypeName::Address,
                Token::TypeString     => ElementaryTypeName::String,
                Token::TypeByte       => ElementaryTypeName::Byte(size.0),
                Token::TypeInt        => ElementaryTypeName::Int(size.0),
                Token::TypeUint       => ElementaryTypeName::Uint(size.0),
                Token::TypeFixed      => ElementaryTypeName::Fixed(size.0, size.1),
                Token::TypeUfixed     => ElementaryTypeName::Ufixed(size.0, size.1),
                _                     => return None,
            }
        };

        self.node_at_token(elementary)
    }

    pub fn variable_declaration(&mut self) -> Option<VariableDeclarationNode<'ast>> {
        // FIXME: context should be passed from the statement parser
        let type_name = self.type_name_in_statement()?;

        let location = match self.lexer.token {
            Token::KeywordStorage => self.node_at_token(StorageLocation::Storage),
            Token::KeywordMemory  => self.node_at_token(StorageLocation::Memory),
            _                     => None,
        };

        let id = self.expect_str_node(Token::Identifier);

        self.node_at(type_name.start, id.end, VariableDeclaration {
            type_name,
            location,
            id,
        })
    }

    fn user_defined_type(&mut self) -> Option<TypeNameNode<'ast>> {
        let (start, end) = self.lexer.loc();
        let identifier = self.lexer.token_as_str();

        self.lexer.consume();

        self.node_at(start, end, identifier)
    }

    fn mapping(&mut self) -> Option<TypeNameNode<'ast>> {
        let start = self.lexer.start_then_consume();

        self.expect(Token::ParenOpen);

        let from = expect!(self, self.elementary_type_name());

        self.expect(Token::Arrow);

        let to  = expect!(self, self.type_name());
        let end = self.expect_end(Token::ParenClose);

        self.node_at(start, end, Mapping {
            from,
            to,
        })
    }
}
