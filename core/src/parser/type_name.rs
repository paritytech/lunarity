use ast::*;
use parser::Parser;
use lexer::Token;

pub trait TypeNameContext<'ast> {
    fn parse(&mut Parser<'ast>) -> Option<TypeNameNode<'ast>>;
}

pub struct RegularTypeNameContext;
pub struct StatementTypeNameContext;

impl<'ast> TypeNameContext<'ast> for RegularTypeNameContext {
    fn parse(par: &mut Parser<'ast>) -> Option<TypeNameNode<'ast>> {
        match par.lexer.token {
            Token::KeywordMapping => par.mapping(),
            Token::Identifier     => par.user_defined_type(),
            _                     => par.elementary_type_name(),
        }
    }
}


impl<'ast> TypeNameContext<'ast> for StatementTypeNameContext {
    fn parse(par: &mut Parser<'ast>) -> Option<TypeNameNode<'ast>> {
        match par.lexer.token {
            Token::KeywordMapping => par.mapping(),
            _                     => par.elementary_type_name(),
        }
    }
}

impl<'ast> Parser<'ast> {
    #[inline]
    pub fn type_name<Context>(&mut self) -> Option<TypeNameNode<'ast>>
    where
        Context: TypeNameContext<'ast>,
    {
        Context::parse(self)
    }

    #[inline]
    pub fn elementary_type_name<E>(&mut self) -> Option<Node<'ast, E>>
    where
        E: From<ElementaryTypeName> + Copy,
    {
        match self.lexer.token {
            Token::TypeBool       => self.bool_type_name(),
            Token::TypeAddress    => self.address_type_name(),
            Token::TypeString     => self.string_type_name(),
            Token::TypeByte       => self.byte_type_name(),
            Token::TypeInt        => self.int_type_name(),
            Token::TypeUint       => self.uint_type_name(),
            Token::TypeFixed      => self.fixed_type_name(),
            Token::TypeUfixed     => self.ufixed_type_name(),
            _                     => self.none(),
        }
    }

    #[inline]
    pub fn none<A>(&mut self) -> Option<Node<'ast, A>>
    where
        A: 'ast,
    {
        None
    }

    #[inline]
    pub fn bool_type_name<E, R>(&mut self) -> R
    where
        E: From<ElementaryTypeName> + Copy + 'ast,
        R: From<Node<'ast, E>>,
    {
        From::from(self.node_at_token(ElementaryTypeName::Bool))
    }

    #[inline]
    pub fn address_type_name<E, R>(&mut self) -> R
    where
        E: From<ElementaryTypeName> + Copy + 'ast,
        R: From<Node<'ast, E>>,
    {
        From::from(self.node_at_token(ElementaryTypeName::Address))
    }

    #[inline]
    pub fn string_type_name<E, R>(&mut self) -> R
    where
        E: From<ElementaryTypeName> + Copy + 'ast,
        R: From<Node<'ast, E>>,
    {
        From::from(self.node_at_token(ElementaryTypeName::String))
    }

    #[inline]
    pub fn byte_type_name<E, R>(&mut self) -> R
    where
        E: From<ElementaryTypeName> + Copy + 'ast,
        R: From<Node<'ast, E>>,
    {
        let bits = self.lexer.type_size.0;

        From::from(self.node_at_token(ElementaryTypeName::Byte(bits)))
    }

    #[inline]
    pub fn int_type_name<E, R>(&mut self) -> R
    where
        E: From<ElementaryTypeName> + Copy + 'ast,
        R: From<Node<'ast, E>>,
    {
        let bits = self.lexer.type_size.0;

        From::from(self.node_at_token(ElementaryTypeName::Int(bits)))
    }

    #[inline]
    pub fn uint_type_name<E, R>(&mut self) -> R
    where
        E: From<ElementaryTypeName> + Copy + 'ast,
        R: From<Node<'ast, E>>,
    {
        let bits = self.lexer.type_size.0;

        From::from(self.node_at_token(ElementaryTypeName::Uint(bits)))
    }

    #[inline]
    pub fn fixed_type_name<E, R>(&mut self) -> R
    where
        E: From<ElementaryTypeName> + Copy + 'ast,
        R: From<Node<'ast, E>>,
    {
        let (bits, precision) = self.lexer.type_size;

        From::from(self.node_at_token(ElementaryTypeName::Fixed(bits, precision)))
    }

    #[inline]
    pub fn ufixed_type_name<E, R>(&mut self) -> R
    where
        E: From<ElementaryTypeName> + Copy + 'ast,
        R: From<Node<'ast, E>>,
    {
        let (bits, precision) = self.lexer.type_size;

        From::from(self.node_at_token(ElementaryTypeName::Ufixed(bits, precision)))
    }

    #[inline]
    pub fn variable_declaration<Context>(&mut self) -> Option<VariableDeclarationNode<'ast>>
    where
        Context: TypeNameContext<'ast>,
    {
        let type_name = self.type_name::<Context>()?;

        Some(self.variable_declaration_from::<Context>(type_name))
    }

    #[inline]
    pub fn variable_declaration_from<Context>(&mut self, type_name: TypeNameNode<'ast>) -> VariableDeclarationNode<'ast> {
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

    #[inline]
    fn user_defined_type(&mut self) -> Option<TypeNameNode<'ast>> {
        let (start, end) = self.lexer.loc();
        let identifier = self.lexer.token_as_str();

        self.lexer.consume();

        self.node_at(start, end, identifier)
    }

    #[inline]
    fn mapping(&mut self) -> Option<TypeNameNode<'ast>> {
        let start = self.lexer.start_then_consume();

        self.expect(Token::ParenOpen);

        let from = expect!(self, self.elementary_type_name());

        self.expect(Token::Arrow);

        let to  = expect!(self, self.type_name::<RegularTypeNameContext>());
        let end = self.expect_end(Token::ParenClose);

        self.node_at(start, end, Mapping {
            from,
            to,
        })
    }
}
