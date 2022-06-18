#![feature(box_patterns, assert_matches)]

use std::env;
use std::error::Error;
use std::io::BufRead;
use std::{
    io::{stdin, stdout, Write},
    process,
};

mod irs;
mod stages;

pub use irs::*;
pub use stages::*;

#[cfg(test)]
mod tests;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Lex,
    Output,
    Run,
}

fn main() {
    let args = env::args();
    let mut mode = Mode::Run;
    for arg in args {
        match &*arg {
            "lex" => mode = Mode::Lex,
            "output" => mode = Mode::Output,
            _ => {}
        }
    }

    if let Err(err) = repl(mode) {
        eprintln!("{}", err);
        process::exit(1);
    }
}

fn repl(mode: Mode) -> Result<(), Box<dyn Error>> {
    let mut line = String::new();
    loop {
        line.clear();
        print!("\x1b[34m>\x1b[0m ");
        stdout().flush()?;
        stdin().lock().read_line(&mut line)?;
        if line.is_empty() {
            println!();
            break Ok(());
        }

        let mut lexer = lexing::Lexer::new(line.chars().peekable()).peekable();

        if mode == Mode::Lex {
            for token in lexer {
                println!("{:?}", token);
            }
            continue;
        }

        let expr = parsing::parse_expr(&mut lexer);
        if let Some(token) = lexer.peek() {
            println!("Warning: expected EOF, found {:?}", token);
        }

        let instrs = compilation::Compilation::compile(&expr);

        if mode == Mode::Output {
            codegen::output_to_file(&instrs, "output.o");
            return Ok(());
        }

        if mode == Mode::Run {
            let res = codegen::run_jit(&instrs);
            println!("{}", res);
            continue;
        }

        unimplemented!("Unimplemented mode {:?}", mode)
    }
}
