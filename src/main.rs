#![feature(box_patterns, assert_matches)]

use std::error::Error;
use std::io::BufRead;
use std::{
    io::{stdin, stdout, Write},
    ops::Range,
    process,
};

use evaluation::Eval;
use lexing::Lexer;
use parsing::Expr;

pub mod evaluation;
pub mod lexing;
pub mod parsing;

#[cfg(test)]
mod tests;

pub type Span = Range<usize>;

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
        print!("\x1b[35mλ\x1b[0m ");
        stdout().flush()?;
        stdin().lock().read_line(&mut line)?;
        if line.is_empty() {
            println!();
            break Ok(());
        }
        let mut lexer = Lexer::new(line.chars().peekable()).peekable();
        let expr = Expr::parse(&mut lexer);
        if let Some(token) = lexer.peek() {
            println!("Warning: expected EOF, found {:?}", token);
        }
        let val = expr.eval();
        println!("{}", val);
    }
}
