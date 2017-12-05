#![feature(test)]

extern crate test;
extern crate toolshed;
extern crate lunaris;

use test::Bencher;

static SOURCE: &'static str = include_str!("./second-price-auction.sol");

#[bench]
fn tokenize(b: &mut Bencher) {
    let arena = toolshed::Arena::new();
    let ptr = arena.alloc_str_with_nul(SOURCE);
    b.bytes = SOURCE.len() as u64;

    b.iter(|| {
        let mut lexer = unsafe { lunaris::lexer::Lexer::from_ptr(ptr) };

        while lexer.token != lunaris::lexer::Token::EndOfProgram {
            lexer.advance()
        }
    });
}
