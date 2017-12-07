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
                Token::DeclarationVar => ElementaryTypeName::Var,
                Token::TypeByte       => ElementaryTypeName::Byte(size.0),
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
