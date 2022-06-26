#![feature(box_patterns, assert_matches)]

use std::collections::HashMap;
use std::env;
use std::error::Error;
use std::io::BufRead;
use std::{
    io::{stdin, stdout, Write},
    process,
};

mod diagnostic;
mod irs;
mod stages;

pub use diagnostic::Diagnostic;
pub use irs::{bytecode, common, syntax_tree, tokens};
pub use stages::{codegen, compilation, lexing, parsing};

#[cfg(test)]
mod tests;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Lex,
    Parse,
    Object,
    Bytecode,
    Run,
}

fn main() {
    let args = env::args();
    let mut mode = Mode::Run;
    for arg in args {
        match &*arg {
            "lex" => mode = Mode::Lex,
            "parse" => mode = Mode::Parse,
            "bytecode" => mode = Mode::Bytecode,
            "object" => mode = Mode::Object,
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
    let mut scope = HashMap::new();
    loop {
        line.clear();
        print!("\x1b[34m>\x1b[0m ");
        stdout().flush()?;
        stdin().lock().read_line(&mut line)?;
        if line.is_empty() {
            println!();
            break Ok(());
        }

        let mut lexer_diagnostics = Vec::new();
        let mut lexer =
            lexing::Lexer::new(line.chars().peekable(), &mut lexer_diagnostics).peekable();

        if mode == Mode::Lex {
            for token in lexer {
                println!("{:?}", token);
            }
            continue;
        }

        let mut parsing_diagnostics = Vec::new();
        let stmt = parsing::Parser::new(&mut lexer, &mut parsing_diagnostics).stmt();
        if let Some(span) = lexer.peek().map(|t| t.span) {
            if parsing_diagnostics.is_empty() {
                lexer_diagnostics.push(Diagnostic::warning(span, "Unexpected token, expected EOF"))
            }
        }

        lexer_diagnostics.append(&mut parsing_diagnostics);
        let diagnostics = lexer_diagnostics;

        if !diagnostics.is_empty() {
            let mut got_err = false;
            for d in diagnostics {
                eprintln!("{}", d);
                if d.level == diagnostic::Level::Error {
                    got_err = true;
                }
            }
            if got_err {
                continue;
            }
        }

        if mode == Mode::Parse {
            println!("{:#?}", stmt);
            continue;
        }

        let func = compilation::compile_with_scope(&[stmt.clone()], &scope);

        if mode == Mode::Bytecode {
            for (i, block) in func.blocks.iter().enumerate() {
                println!("block{}:", i);
                for instr in &block.instrs {
                    println!("    {:?}", instr);
                }
            }
            continue;
        }

        if mode == Mode::Object {
            codegen::output_to_file(&func, "output.o");
            return Ok(());
        }

        if mode == Mode::Run {
            let res = codegen::run_jit(&func);
            if let syntax_tree::Stmt::Assign(assign) = stmt {
                match assign.assignee {
                    syntax_tree::Assignee::Let(common::Ident { name, .. })
                    | syntax_tree::Assignee::Expr(syntax_tree::Expr::Ident(common::Ident {
                        name,
                        ..
                    })) => {
                        scope.insert(name, res);
                    }
                    _ => {}
                }
            }
            println!("{}", res);
            continue;
        }

        unimplemented!("Unimplemented mode {:?}", mode)
    }
}
