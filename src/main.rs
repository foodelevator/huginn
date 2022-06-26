#![feature(box_patterns, assert_matches)]

use std::collections::HashMap;
use std::error::Error;
use std::io::BufRead;
use std::ops::ControlFlow;
use std::{env, fs};
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
    Build,
    Bytecode,
    Run,
}

fn main() {
    let args = env::args();
    let mut mode = Mode::Run;
    let mut path = None;
    for arg in args.skip(1) {
        match &*arg {
            "lex" => mode = Mode::Lex,
            "parse" => mode = Mode::Parse,
            "bytecode" => mode = Mode::Bytecode,
            "build" => mode = Mode::Build,
            "run" => mode = Mode::Run,
            name => path = Some(name.to_string()),
        }
    }

    if let Some(path) = path {
        match fs::read_to_string(path) {
            Ok(src_code) => {
                if let Err(err) = run(&src_code, mode, &mut HashMap::new()) {
                    eprintln!("{}", err);
                    process::exit(1);
                }
            }
            Err(err) => {
                eprintln!("{}", err);
                process::exit(1);
            }
        }
    } else {
        if let Err(err) = repl(mode) {
            eprintln!("{}", err);
            process::exit(1);
        }
    }
}

fn run(
    src: &str,
    mode: Mode,
    scope: &mut HashMap<String, i64>,
) -> Result<ControlFlow<()>, Box<dyn Error>> {
    let mut lexer_diagnostics = Vec::new();
    let mut lexer = lexing::Lexer::new(src.chars().peekable(), &mut lexer_diagnostics).peekable();

    if mode == Mode::Lex {
        for token in lexer {
            println!("{:?}", token);
        }
        return Ok(ControlFlow::Continue(()));
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
        for d in diagnostics {
            eprintln!("{}", d.display(src));
        }
    }

    let stmt = if let Some(stmt) = stmt {
        stmt
    } else {
        return Ok(ControlFlow::Continue(()));
    };

    if mode == Mode::Parse {
        println!("{:#?}", stmt);
        return Ok(ControlFlow::Continue(()));
    }

    let func = compilation::compile_stmt(&stmt, &scope);

    if mode == Mode::Bytecode {
        for (i, block) in func.blocks.iter().enumerate() {
            println!("block{}:", i);
            for instr in &block.instrs {
                println!("    {:?}", instr);
            }
        }
        return Ok(ControlFlow::Continue(()));
    }

    if mode == Mode::Build {
        codegen::output_to_file(&func, "output.o");
        return Ok(ControlFlow::Break(()));
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
        return Ok(ControlFlow::Continue(()));
    }

    unimplemented!("Unimplemented mode {:?}", mode)
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

        match run(&line, mode, &mut scope)? {
            ControlFlow::Continue(_) => continue,
            ControlFlow::Break(_) => break Ok(()),
        }
    }
}
