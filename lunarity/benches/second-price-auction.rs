#![feature(test)]

extern crate test;
extern crate toolshed;
extern crate lunarity;

use lunarity::lexer::{Lexer, Token};
use lunarity::parse;

use test::{Bencher, black_box};

static SOURCE: &'static str = include_str!("./second-price-auction.sol");

#[bench]
fn tokenize(b: &mut Bencher) {
    let arena = toolshed::Arena::new();
    let nts = arena.alloc_nul_term_str(SOURCE);
    b.bytes = SOURCE.len() as u64;

    b.iter(|| {
        let mut lexer = Lexer::new(nts);

        while lexer.token != Token::EndOfProgram {
            lexer.advance()
        }
    });
}

#[bench]
fn parse_to_ast(b: &mut Bencher) {
    b.bytes = SOURCE.len() as u64;

    b.iter(|| {
        let program = parse(SOURCE);

        black_box(program.unwrap())
    });
}
