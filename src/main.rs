#![feature(box_patterns, assert_matches)]

use std::io::BufRead;
use std::{
    io::{stdin, stdout, Write},
    ops::Range,
    process,
};

use crate::lexing::Lexer;
use crate::parsing::Expr;

pub mod lexing;
pub mod parsing;

#[cfg(test)]
mod tests;

pub type Span = Range<usize>;

fn main() {
    let mut line = String::new();
    loop {
        line.clear();
        print!("\x1b[35mλ\x1b[0m ");
        let _ = stdout().flush();
        if let Err(err) = stdin().lock().read_line(&mut line) {
            eprintln!("Error reading stdin: {}", err);
            process::exit(1);
        }
        if line.is_empty() {
            println!();
            break;
        }
        let mut lexer = Lexer::new(line.chars().peekable()).peekable();
        let expr = Expr::parse(&mut lexer);
        if let Some(token) = lexer.peek() {
            println!("Warning: expected EOF, found {:?}", token);
        }
        println!("{:#?}", expr);
    }
}
