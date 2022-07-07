# huginn

A hobby project I'm working on when I have time and energy after work.

This is a compiler for a (so far) very simple programming language.

This currently uses
[cranelift](https://github.com/bytecodealliance/wasmtime/tree/main/cranelift)
for code generation.

## Quickstart

To run in JIT mode:
```
$ cargo run run src/tests/fibonacci.hg
```

To build a static executable:
```
$ cargo run build src/tests/fibonacci.hg
$ ./a.out
```
