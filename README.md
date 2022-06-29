# compiler

A hobby project I'm working on when I have time and energy after work.

This currently uses [cranelift](https://github.com/bytecodealliance/wasmtime/tree/main/cranelift)
for code generation.

Appaerently cranelift doesn't allow syscalls, so currently printing numbers as
well as exit are implemented in assembly.
