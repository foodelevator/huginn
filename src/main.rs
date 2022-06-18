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
    Bytecode,
    Run,
}

fn main() {
    let args = env::args();
    let mut mode = Mode::Run;
    for arg in args {
        match &*arg {
            "lex" => mode = Mode::Lex,
            "bytecode" => mode = Mode::Bytecode,
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

        let func = compilation::compile(&expr);

        if mode == Mode::Bytecode {
            for (i, block) in func.blocks.iter().enumerate() {
                println!("block{}:", i);
                for instr in &block.instrs {
                    println!("    {:?}", instr);
                }
            }
            continue;
        }

        if mode == Mode::Output {
            codegen::output_to_file(&func, "output.o");
            return Ok(());
        }

        if mode == Mode::Run {
            let res = codegen::run_jit(&func);
            println!("{}", res);
            continue;
        }

        unimplemented!("Unimplemented mode {:?}", mode)
    }
}
