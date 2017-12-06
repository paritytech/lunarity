use toolshed::Arena;

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

    pub fn node<'a, T, I, R>(&'a self, start: u32, end: u32, val: I) -> R
    where
        T: 'a + Copy,
        I: Into<T>,
        R: From<Node<'a, T>>,
    {
        Node::new(self.arena.alloc(NodeInner::new(start, end, val.into()))).into()
    }

    pub fn list<'a, T, L>(&'a self, list: L) -> NodeList<'a, T> where
        T: 'a + Copy,
        L: AsRef<[Node<'a, T>]>,
    {
        NodeList::from_iter(&self.arena, list.as_ref().iter().cloned())
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
