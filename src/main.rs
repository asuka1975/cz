mod ast;
mod codegen;
mod lexer;
mod parser;
mod semantic;
mod token;

use inkwell::context::Context;
use std::env;
use std::fs;
use std::process::Command;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("使い方: czc <ソースファイル.cz> [-o <出力ファイル>] [--emit-llvm]");
        std::process::exit(1);
    }

    let source_file = &args[1];
    let mut output_file = "a.out".to_string();
    let mut emit_llvm = false;

    let mut i = 2;
    while i < args.len() {
        match args[i].as_str() {
            "-o" => {
                i += 1;
                if i < args.len() {
                    output_file = args[i].clone();
                }
            }
            "--emit-llvm" => {
                emit_llvm = true;
            }
            _ => {}
        }
        i += 1;
    }

    let source = match fs::read_to_string(source_file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("エラー: ファイル '{}' を読み込めません: {}", source_file, e);
            std::process::exit(1);
        }
    };

    // Lexer
    let mut lexer = lexer::Lexer::new(&source);
    let tokens = match lexer.tokenize() {
        Ok(t) => t,
        Err(e) => {
            eprintln!("字句解析エラー: {}", e);
            std::process::exit(1);
        }
    };

    // Parser
    let mut parser = parser::Parser::new(tokens);
    let program = match parser.parse_program() {
        Ok(p) => p,
        Err(e) => {
            eprintln!("構文解析エラー: {}", e);
            std::process::exit(1);
        }
    };

    // Semantic analysis
    let mut analyzer = semantic::SemanticAnalyzer::new();
    if let Err(errors) = analyzer.analyze(&program) {
        for e in &errors {
            eprintln!("意味解析エラー: {}", e);
        }
        std::process::exit(1);
    }

    // Code generation (Cz -> LLVM IR via inkwell)
    let context = Context::create();
    let mut codegen = codegen::CodeGen::new(&context);
    codegen.generate(&program);

    if emit_llvm {
        println!("{}", codegen.print_to_string());
        return;
    }

    // Write object file
    let obj_file = format!("{}.o", output_file);
    if let Err(e) = codegen.write_object_file(std::path::Path::new(&obj_file)) {
        eprintln!("エラー: オブジェクトファイルの生成に失敗: {}", e);
        std::process::exit(1);
    }

    // Link with clang
    let clang_result = Command::new("clang")
        .args([&obj_file, "-o", &output_file])
        .output();

    let _ = fs::remove_file(&obj_file);

    match clang_result {
        Ok(output) => {
            if !output.status.success() {
                let stderr = String::from_utf8_lossy(&output.stderr);
                eprintln!("リンクエラー: {}", stderr);
                std::process::exit(1);
            }
        }
        Err(e) => {
            eprintln!("エラー: clang の実行に失敗: {}", e);
            std::process::exit(1);
        }
    }
}
