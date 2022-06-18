#![feature(box_patterns, assert_matches)]

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

fn main() {
    if let Err(err) = repl() {
        eprintln!("{}", err);
        process::exit(1);
    }
}

fn repl() -> Result<(), Box<dyn Error>> {
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
        let expr = parsing::parse_expr(&mut lexer);
        if let Some(token) = lexer.peek() {
            println!("Warning: expected EOF, found {:?}", token);
        }
        let instrs = compilation::Compilation::compile(&expr);
        let res = codegen::run_jit(&instrs);
        println!("{}", res);
    }
}
