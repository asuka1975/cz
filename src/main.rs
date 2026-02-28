mod analysis;
mod arena;
mod codegen;
mod diagnostics;
mod hir;
mod scope;
mod syntax;

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
    let mut lexer = syntax::lexer::Lexer::new(&source);
    let tokens = match lexer.tokenize() {
        Ok(t) => t,
        Err(e) => {
            eprintln!("字句解析エラー: {}", e);
            std::process::exit(1);
        }
    };

    // Parser
    let parser = syntax::parser::Parser::new(tokens);
    let parse_result = match parser.parse_program() {
        Ok(r) => r,
        Err(e) => {
            eprintln!("構文解析エラー: {}", e);
            std::process::exit(1);
        }
    };

    // Type context (register structs, enums, functions)
    let type_ctx = match hir::types::TypeContext::build(&parse_result.program, &source) {
        Ok(ctx) => ctx,
        Err(errors) => {
            for e in &errors {
                eprintln!("意味解析エラー: {}", e);
            }
            std::process::exit(1);
        }
    };

    // Lowering (AST → HIR: type inference + name resolution)
    let lowering = hir::lower::Lowering::new(&type_ctx, &source);
    let lower_result = match lowering.lower(
        &parse_result.program,
        &parse_result.expr_arena,
        &parse_result.stmt_arena,
    ) {
        Ok(r) => r,
        Err(errors) => {
            for e in &errors {
                eprintln!("{}", e);
            }
            std::process::exit(1);
        }
    };

    // Semantic analysis (on HIR)
    {
        let mut analyzer = analysis::SemanticAnalyzer::new(
            &type_ctx,
            &lower_result.expr_arena,
            &lower_result.stmt_arena,
            &lower_result.vars,
            &lower_result.func_names,
            &source,
        );
        if let Err(errors) = analyzer.analyze(&lower_result.program) {
            for e in &errors {
                eprintln!("{}", e);
            }
            std::process::exit(1);
        }
    }

    // Code generation (HIR → LLVM IR via inkwell)
    let context = Context::create();
    let hir::LowerResult {
        program: hir_program,
        expr_arena,
        stmt_arena,
        vars,
        func_names,
    } = lower_result;
    let mut codegen =
        codegen::CodeGen::new(&context, expr_arena, stmt_arena, vars, func_names, type_ctx);
    if let Err(e) = codegen.generate(&hir_program) {
        eprintln!("コード生成エラー: {}", e);
        std::process::exit(1);
    }

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
