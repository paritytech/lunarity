# Lunarity

A high performance, correctness-oriented Solidity parser + other tools.

The produced AST is mostly [grammar.txt](https://github.com/ethereum/solidity/blob/develop/docs/grammar.txt) compliant.
The AST is statically typed to make sure that it is not possible to construct an AST representation of what would be
illegal grammar.

## Performance

It's *really* fast.

```
Running target/release/deps/second_price_auction-2b369ce54b97fb9f

running 2 tests
test parse_to_ast ... bench:      24,028 ns/iter (+/- 573) = 562 MB/s
test tokenize     ... bench:      15,037 ns/iter (+/- 1,405) = 898 MB/s
```

Neither the lexer nor the parser ever backtrack while reading bytes/tokens. The parser is using [a paginated
arena allocator](https://docs.rs/toolshed/0.4.0/toolshed/struct.Arena.html) to avoid heap allocation costs
(or garbage collection costs, or anything else).

Feel free to rerun the benchmarks (requires nightly Rust, with rustup: `rustup run nightly cargo bench`)

## License

This crate is distributed under the terms of GNU GENERAL PUBLIC LICENSE version 3.0.

See [LICENSE](LICENSE) for details.
