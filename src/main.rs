use std::error::Error;
use std::io::BufRead;
use std::io::{stdin, stdout, Write};
use std::ops::ControlFlow;
use std::path::Path;
use std::{collections::HashMap, env, fs, process};

use compiler::common::Ident;
use compiler::compilation::{compile_block, compile_stmt};
use compiler::lexing::Lexer;
use compiler::parsing::Parser;
use compiler::syntax_tree::{Assignee, Expr, Stmt};
use compiler::{codegen, Diagnostic};

fn main() {
    let args = env::args();
    let mut mode = Mode::Run;
    let mut path = None;
    for arg in args.skip(1) {
        match &*arg {
            "parse" => mode = Mode::Parse,
            "bytecode" => mode = Mode::Bytecode,
            "build" => mode = Mode::Build,
            "run" => mode = Mode::Run,
            name => path = Some(name.to_string()),
        }
    }

    if let Some(path) = path {
        if let Err(err) = run_file(path, mode) {
            eprintln!("{}", err);
            process::exit(1);
        }
    } else {
        if let Err(err) = repl(mode) {
            eprintln!("{}", err);
            process::exit(1);
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    Parse,
    Build,
    Bytecode,
    Run,
}

pub fn repl(mode: Mode) -> Result<(), Box<dyn Error>> {
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

        match run_stmt(&line, mode, &mut scope)? {
            ControlFlow::Continue(_) => continue,
            ControlFlow::Break(_) => break Ok(()),
        }
    }
}

pub fn run_stmt(
    src: &str,
    mode: Mode,
    scope: &mut HashMap<String, i64>,
) -> Result<ControlFlow<()>, Box<dyn Error>> {
    let mut lexer_diagnostics = Vec::new();
    let mut lexer = Lexer::new(src.chars().peekable(), &mut lexer_diagnostics).peekable();

    let mut parsing_diagnostics = Vec::new();
    let stmt = Parser::new(&mut lexer, &mut parsing_diagnostics).stmt();
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

    let func = compile_stmt(&stmt, &scope);

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
        if let Stmt::Assign(assign) = stmt {
            match assign.assignee {
                Assignee::Let(Ident { name, .. })
                | Assignee::Expr(Expr::Ident(Ident { name, .. })) => {
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

pub fn run_file(path: impl AsRef<Path>, mode: Mode) -> Result<(), Box<dyn Error>> {
    let src = match fs::read_to_string(path) {
        Ok(src) => format!("{{{}}}", src),
        Err(err) => return Err(Box::new(err)),
    };

    let mut lexer_diagnostics = Vec::new();
    let mut lexer = Lexer::new(src.chars().peekable(), &mut lexer_diagnostics).peekable();

    let mut parsing_diagnostics = Vec::new();
    let block = Parser::new(&mut lexer, &mut parsing_diagnostics).block();
    if let Some(span) = lexer.peek().map(|t| t.span) {
        if parsing_diagnostics.is_empty() {
            lexer_diagnostics.push(Diagnostic::warning(span, "Unexpected token, expected EOF"))
        }
    }

    lexer_diagnostics.append(&mut parsing_diagnostics);
    let diagnostics = lexer_diagnostics;

    if !diagnostics.is_empty() {
        for d in diagnostics {
            eprintln!("{}", d.display(&src));
        }
    }

    let block = if let Some(block) = block {
        block
    } else {
        return Ok(());
    };

    if mode == Mode::Parse {
        println!("{:#?}", block);
        return Ok(());
    }

    let func = compile_block(&block);

    if mode == Mode::Bytecode {
        for (i, block) in func.blocks.iter().enumerate() {
            println!("block{}:", i);
            for instr in &block.instrs {
                println!("    {:?}", instr);
            }
        }
        return Ok(());
    }

    if mode == Mode::Build {
        codegen::output_to_file(&func, "output.o");
        return Ok(());
    }

    if mode == Mode::Run {
        let res = codegen::run_jit(&func);
        println!("{}", res);
        return Ok(());
    }

    unimplemented!("Unimplemented mode {:?}", mode)
}
