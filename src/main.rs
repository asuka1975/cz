mod ast;
mod codegen;
mod lexer;
mod parser;

use std::process;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: cz <source-file>");
        process::exit(1);
    }

    let source = std::fs::read_to_string(&args[1]).unwrap_or_else(|e| {
        eprintln!("error: could not read '{}': {e}", args[1]);
        process::exit(1);
    });

    let tokens = lexer::tokenize(&source).unwrap_or_else(|e| {
        eprintln!("lex error: {e}");
        process::exit(1);
    });

    let program = parser::parse(&tokens).unwrap_or_else(|e| {
        eprintln!("parse error: {e}");
        process::exit(1);
    });

    let output = codegen::generate(&program).unwrap_or_else(|e| {
        eprintln!("codegen error: {e}");
        process::exit(1);
    });

    print!("{output}");
}
