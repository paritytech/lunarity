use toolshed::Arena;
use toolshed::list::List;

use ast::*;

pub struct Mock {
    arena: Arena
}

impl Mock {
    pub fn new() -> Self {
        Mock {
            arena: Arena::new()
        }
    }

    pub fn node<'mock, T, I, R>(&'mock self, start: u32, end: u32, val: I) -> R
    where
        T: 'mock + Copy,
        I: Into<T>,
        R: From<Node<'mock, T>>,
    {
        Node::new(self.arena.alloc(NodeInner::new(start, end, val.into()))).into()
    }

    /// A variant of `node` to keep the type inference clean
    pub fn stmt_expr<'mock, I, R>(&'mock self, start: u32, e_end: u32, s_end: u32, val: I) -> R
    where
        I: Into<Expression<'mock>>,
        R: From<StatementNode<'mock>>,
    {
        let expression = Node::new(self.arena.alloc(NodeInner::new(start, e_end, val.into())));

        Node::new(self.arena.alloc(NodeInner::new(start, s_end, expression.into()))).into()
    }

    pub fn list<'mock, T, L>(&'mock self, list: L) -> List<T> where
        T: 'mock + Copy,
        L: AsRef<[T]>,
    {
        List::from_iter(&self.arena, list.as_ref().iter().cloned())
    }
}


pub fn assert_units<'mock, E>(source: &str, expected: E)
where
    E: AsRef<[SourceUnitNode<'mock>]>
{
    use parser::parse;

    let program = parse(source).unwrap();

    let iter = program
                .body()
                .iter()
                .zip(expected.as_ref().iter());

    for (got, expected) in iter {
        assert_eq!(got, expected);
    }

    let got = program.body().iter().count();
    let expected = expected.as_ref().iter().count();

    assert_eq!(got, expected, "Expected {} units, got {}", expected, got);
}
