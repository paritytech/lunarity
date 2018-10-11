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
    let ptr = arena.alloc_str_with_nul(SOURCE);
    b.bytes = SOURCE.len() as u64;

    b.iter(|| {
        let mut lexer = unsafe { Lexer::from_ptr(ptr) };

        while lexer.token != Token::EndOfProgram {
            lexer.consume()
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
