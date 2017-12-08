use ast::*;
use parser::Parser;
use lexer::Token;

impl<'ast> Parser<'ast> {
    pub fn type_name(&mut self) -> Option<TypeNameNode<'ast>> {
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
        let type_name = self.type_name()?;

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
}
