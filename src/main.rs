use std::error::Error;
use std::io::stdin;
use std::io::Read;
use std::{env, fs, process};

use huginn::analysis::analyze_mod;
use huginn::lexing::Lexer;
use huginn::link::link;
use huginn::lowering::lower_file;
use huginn::parsing::Parser;
use huginn::{codegen, Diagnostic};

fn main() {
    let args = env::args();
    let mut mode = Mode::Parse;
    let mut path = None;
    for arg in args.skip(1) {
        match &*arg {
            "parse" => mode = Mode::Parse,
            "bytecode" => mode = Mode::Bytecode,
            "bitcode" => mode = Mode::Bitcode,
            "object" => mode = Mode::Object,
            "build" => mode = Mode::Build,
            "run" => mode = Mode::Run,
            name if path.is_none() => path = Some(name.to_string()),
            arg => {
                eprintln!(
                    "Unknown command {}, only one source file can be specified",
                    arg
                );
                process::exit(1);
            }
        }
    }

    if let Some(path) = path {
        let file = match fs::File::open(&path) {
            Ok(file) => file,
            Err(err) => {
                eprintln!("{}", err);
                process::exit(1);
            }
        };
        if let Err(err) = handle_file(file, &path, mode) {
            eprintln!("{}", err);
            process::exit(1);
        }
    } else {
        let input = stdin();
        if let Err(err) = handle_file(input, "<stdin>", mode) {
            eprintln!("{}", err);
            process::exit(1);
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    Parse,
    Object,
    Build,
    Bytecode,
    Bitcode,
    Run,
}

pub fn handle_file(mut input: impl Read, filename: &str, mode: Mode) -> Result<(), Box<dyn Error>> {
    let mut src = String::new();
    input.read_to_string(&mut src)?;
    let src = src;

    let mut lexer = Lexer::new(src.chars().peekable(), 0);
    let mut parser = Parser::new(&mut lexer);
    let file = parser.file();

    let got_parsing_errors = if !parser.diagnostics().is_empty() {
        for d in parser.diagnostics() {
            eprintln!("{}", d.display(&src, |_| filename));
        }
        true
    } else {
        false
    };

    if let Some(span) = lexer.peek().map(|t| t.span) {
        if !got_parsing_errors {
            eprintln!(
                "{}",
                Diagnostic::warning(span, "Unexpected token, expected EOF")
                    .display(&src, |_| filename)
            );
        }
    }

    if !lexer.diagnostics().is_empty() {
        for d in lexer.diagnostics() {
            eprintln!("{}", d.display(&src, |_| filename));
        }
    }

    let file = if let Some(file) = file {
        file
    } else {
        return Ok(());
    };

    if mode == Mode::Parse {
        println!("{:#?}", file);
        return Ok(());
    }

    let module = lower_file(&file);

    if mode == Mode::Bytecode {
        for (name, &id) in &module.scope {
            let proc = &module.symbols[id];

            println!("procedure {}:", name);
            for (i, block) in proc.blocks.iter().enumerate() {
                println!("  block{}:", i);
                for instr in &block.instrs {
                    println!("    {:?}", instr);
                }
            }
        }
        return Ok(());
    }

    let module = analyze_mod(&module);

    if mode == Mode::Bitcode {
        for (id, proc) in module.procedures.enumerate() {
            println!("procedure {}:", id);
            for (i, block) in proc.blocks.iter().enumerate() {
                println!("  block{}:", i);
                for instr in &block.instrs {
                    println!("    {:?}", instr);
                }
            }
        }
        return Ok(());
    }

    if mode == Mode::Object || mode == Mode::Build {
        let obj = codegen::build_object(&module);
        if mode == Mode::Object {
            fs::write("output.o", obj)?;
        } else {
            link(obj)?;
        }
        return Ok(());
    }

    // if mode == Mode::Run {
    //     let res = codegen::run_jit(&module);
    //     if res != 0 {
    //         println!("{}", res);
    //     }
    //     return Ok(());
    // }

    unimplemented!("Someone missed something for mode {:?}", mode)
}
