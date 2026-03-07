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
use std::path::Path;
use std::process::Command;

/// コンパイルパイプラインを実行し、オブジェクトファイルを生成する。
/// 成功時は生成されたオブジェクトファイルのパスを返す。
fn compile_to_object(source: &str, obj_path: &Path) -> Result<(), Vec<String>> {
    // Lexer
    let mut lexer = syntax::lexer::Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .map_err(|e| vec![format!("字句解析エラー: {}", e)])?;

    // Parser
    let parser = syntax::parser::Parser::new(tokens);
    let parse_result = parser
        .parse_program()
        .map_err(|e| vec![format!("構文解析エラー: {}", e)])?;

    // Type context
    let type_ctx =
        hir::types::TypeContext::build(&parse_result.program, source).map_err(|errors| {
            errors
                .iter()
                .map(|e| format!("意味解析エラー: {}", e))
                .collect::<Vec<_>>()
        })?;

    // Lowering
    let lowering = hir::lower::Lowering::new(&type_ctx, source);
    let lower_result = lowering
        .lower(
            &parse_result.program,
            &parse_result.expr_arena,
            &parse_result.stmt_arena,
        )
        .map_err(|errors| errors.iter().map(|e| e.to_string()).collect::<Vec<_>>())?;

    // Semantic analysis
    {
        let mut analyzer = analysis::SemanticAnalyzer::new(
            &type_ctx,
            &lower_result.expr_arena,
            &lower_result.stmt_arena,
            &lower_result.vars,
            &lower_result.func_names,
            source,
        );
        analyzer
            .analyze(&lower_result.program)
            .map_err(|errors| errors.iter().map(|e| e.to_string()).collect::<Vec<_>>())?;
    }

    // Code generation
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
    codegen
        .generate(&hir_program)
        .map_err(|e| vec![format!("コード生成エラー: {}", e)])?;

    // Write object file
    codegen
        .write_object_file(obj_path)
        .map_err(|e| vec![format!("オブジェクトファイル生成エラー: {}", e)])?;

    Ok(())
}

/// オブジェクトファイルをリンクして実行ファイルを生成する。
fn link(obj_path: &Path, output_path: &Path) -> Result<(), String> {
    let result = Command::new("clang")
        .args([
            obj_path.to_str().unwrap(),
            "-o",
            output_path.to_str().unwrap(),
        ])
        .output()
        .map_err(|e| format!("clang の実行に失敗: {}", e))?;

    if !result.status.success() {
        let stderr = String::from_utf8_lossy(&result.stderr);
        return Err(format!("リンクエラー: {}", stderr));
    }

    Ok(())
}

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

    if emit_llvm {
        // emit-llvm mode: compile but print IR instead of writing object
        let mut lexer = syntax::lexer::Lexer::new(&source);
        let tokens = match lexer.tokenize() {
            Ok(t) => t,
            Err(e) => {
                eprintln!("字句解析エラー: {}", e);
                std::process::exit(1);
            }
        };
        let parser = syntax::parser::Parser::new(tokens);
        let parse_result = match parser.parse_program() {
            Ok(r) => r,
            Err(e) => {
                eprintln!("構文解析エラー: {}", e);
                std::process::exit(1);
            }
        };
        let type_ctx = match hir::types::TypeContext::build(&parse_result.program, &source) {
            Ok(ctx) => ctx,
            Err(errors) => {
                for e in &errors {
                    eprintln!("意味解析エラー: {}", e);
                }
                std::process::exit(1);
            }
        };
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
        println!("{}", codegen.print_to_string());
        return;
    }

    // Normal compilation
    let obj_file = format!("{}.o", output_file);
    let obj_path = Path::new(&obj_file);

    if let Err(errors) = compile_to_object(&source, obj_path) {
        for e in &errors {
            eprintln!("{}", e);
        }
        std::process::exit(1);
    }

    if let Err(e) = link(obj_path, Path::new(&output_file)) {
        let _ = fs::remove_file(obj_path);
        eprintln!("{}", e);
        std::process::exit(1);
    }

    let _ = fs::remove_file(obj_path);
}

#[cfg(test)]
mod integration_tests {
    use super::*;
    use std::path::PathBuf;

    struct TestMeta {
        expect: String,
        expected_exit_code: Option<i32>,
        expected_stdout: Option<String>,
    }

    fn parse_meta(source: &str) -> TestMeta {
        let mut expect = String::new();
        let mut expected_exit_code = None;
        let mut expected_stdout = None;

        for line in source.lines() {
            let line = line.trim();
            if let Some(v) = line.strip_prefix("// EXPECT: ") {
                expect = v.trim().to_string();
            } else if let Some(v) = line.strip_prefix("// EXPECTED_EXIT_CODE: ") {
                expected_exit_code = v.trim().parse().ok();
            } else if let Some(v) = line.strip_prefix("// EXPECTED_STDOUT: ") {
                let decoded = v.replace("\\n", "\n");
                expected_stdout = Some(decoded);
            }
        }

        TestMeta {
            expect,
            expected_exit_code,
            expected_stdout,
        }
    }

    fn run_cz_test(path: &Path) -> Result<(), String> {
        let source = fs::read_to_string(path)
            .map_err(|e| format!("Failed to read {}: {}", path.display(), e))?;
        let meta = parse_meta(&source);

        let tmpdir =
            tempfile::tempdir().map_err(|e| format!("Failed to create temp dir: {}", e))?;
        let obj_path = tmpdir.path().join("output.o");
        let bin_path = tmpdir.path().join("a.out");

        // Compile to object
        let compile_result = compile_to_object(&source, &obj_path);

        match meta.expect.as_str() {
            "success" => {
                compile_result.map_err(|errors| {
                    format!(
                        "Expected success, got compile error:\n{}",
                        errors.join("\n")
                    )
                })?;

                link(&obj_path, &bin_path).map_err(|e| format!("Expected success, got: {}", e))?;

                let run = Command::new(bin_path.to_str().unwrap())
                    .output()
                    .map_err(|e| format!("Failed to run binary: {}", e))?;

                let run_exit = run.status.code().unwrap_or(-1);
                let run_stdout = String::from_utf8_lossy(&run.stdout).to_string();

                if run_exit >= 128 {
                    return Err(format!(
                        "Expected success, got killed by signal (exit {})\nOutput: {}",
                        run_exit, run_stdout
                    ));
                }

                let mut failures = Vec::new();

                if let Some(expected_exit) = meta.expected_exit_code
                    && run_exit != expected_exit
                {
                    failures.push(format!(
                        "Exit code: expected {}, got {}",
                        expected_exit, run_exit
                    ));
                }

                if let Some(ref expected_stdout) = meta.expected_stdout
                    && run_stdout != *expected_stdout
                {
                    failures.push(format!(
                        "Stdout: expected {:?}, got {:?}",
                        expected_stdout, run_stdout
                    ));
                }

                if !failures.is_empty() {
                    return Err(failures.join("\n"));
                }

                Ok(())
            }
            "compile-error" => {
                if compile_result.is_ok() {
                    Err("Expected compile-error, got success".to_string())
                } else {
                    Ok(())
                }
            }
            "link-error" => match compile_result {
                Ok(()) => match link(&obj_path, &bin_path) {
                    Ok(()) => Err("Expected link-error, got success".to_string()),
                    Err(e) if e.contains("リンクエラー") => Ok(()),
                    Err(e) => Err(format!("Expected link-error, got other error: {}", e)),
                },
                Err(errors) => Err(format!(
                    "Expected link-error, got compile error:\n{}",
                    errors.join("\n")
                )),
            },
            "runtime-error" => {
                compile_result.map_err(|errors| {
                    format!(
                        "Expected runtime-error, got compile error:\n{}",
                        errors.join("\n")
                    )
                })?;

                link(&obj_path, &bin_path)
                    .map_err(|e| format!("Expected runtime-error, got link error: {}", e))?;

                let run = Command::new(bin_path.to_str().unwrap())
                    .output()
                    .map_err(|e| format!("Failed to run binary: {}", e))?;

                let run_exit = run.status.code().unwrap_or(-1);
                if run_exit == 0 {
                    Err("Expected runtime-error, got success (exit 0)".to_string())
                } else {
                    Ok(())
                }
            }
            other => Err(format!("Unknown EXPECT: {}", other)),
        }
    }

    fn collect_cz_files() -> Vec<PathBuf> {
        let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let tests_dir = manifest_dir.join("tests");
        let mut files = Vec::new();

        for subdir in &["lexer", "parser", "typesystem", "codegen", "integration"] {
            let dir = tests_dir.join(subdir);
            if !dir.exists() {
                continue;
            }
            if let Ok(entries) = fs::read_dir(&dir) {
                for entry in entries.flatten() {
                    let path = entry.path();
                    if path.extension().is_some_and(|e| e == "cz") {
                        files.push(path);
                    }
                }
            }
        }

        files.sort();
        files
    }

    #[test]
    fn cz_integration_tests() {
        let files = collect_cz_files();
        assert!(!files.is_empty(), "No .cz test files found");

        let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let mut failures = Vec::new();

        for path in &files {
            let rel = path
                .strip_prefix(&manifest_dir)
                .unwrap_or(path)
                .display()
                .to_string();
            match run_cz_test(path) {
                Ok(()) => eprintln!("  PASS: {}", rel),
                Err(e) => {
                    eprintln!("  FAIL: {}", rel);
                    failures.push(format!("{}: {}", rel, e));
                }
            }
        }

        if !failures.is_empty() {
            panic!(
                "\n{} test(s) failed:\n{}",
                failures.len(),
                failures.join("\n\n")
            );
        }

        eprintln!("\n  All {} integration tests passed.", files.len());
    }
}
