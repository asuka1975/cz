use crate::ast::{BinaryOp, Block, Declaration, Expr, FunctionDecl, Program, Stmt, Type};

/// Generates assembly-like IR text from the given [`Program`].
///
/// This is a placeholder backend that emits a simple human-readable IR so the
/// overall compiler pipeline can be exercised end-to-end.  Replace or extend
/// this module with a real backend (e.g. LLVM IR, x86 assembly, or bytecode)
/// as the language evolves.
pub fn generate(program: &Program) -> Result<String, CodegenError> {
    let mut out = String::new();
    for decl in &program.declarations {
        match decl {
            Declaration::Function(f) => generate_function(&mut out, f)?,
        }
    }
    Ok(out)
}

fn generate_function(out: &mut String, f: &FunctionDecl) -> Result<(), CodegenError> {
    out.push_str(&format!("fn {}(", f.name));
    for (i, param) in f.params.iter().enumerate() {
        if i > 0 {
            out.push_str(", ");
        }
        out.push_str(&format!("{}: {}", param.name, type_name(&param.ty)));
    }
    out.push_str(&format!(") -> {}", type_name(&f.return_type)));
    out.push_str(" {\n");
    generate_block(out, &f.body)?;
    out.push_str("}\n");
    Ok(())
}

fn generate_block(out: &mut String, block: &Block) -> Result<(), CodegenError> {
    for stmt in &block.stmts {
        generate_stmt(out, stmt)?;
    }
    Ok(())
}

fn generate_stmt(out: &mut String, stmt: &Stmt) -> Result<(), CodegenError> {
    match stmt {
        Stmt::Return(None) => out.push_str("  return;\n"),
        Stmt::Return(Some(expr)) => {
            out.push_str(&format!("  return {};\n", expr_to_string(expr)?));
        }
        Stmt::Expr(expr) => {
            out.push_str(&format!("  {};\n", expr_to_string(expr)?));
        }
    }
    Ok(())
}

fn expr_to_string(expr: &Expr) -> Result<String, CodegenError> {
    match expr {
        Expr::Integer(n) => Ok(n.to_string()),
        Expr::Identifier(name) => Ok(name.clone()),
        Expr::BinaryOp { op, lhs, rhs } => {
            let op_str = match op {
                BinaryOp::Add => "+",
                BinaryOp::Sub => "-",
                BinaryOp::Mul => "*",
                BinaryOp::Div => "/",
            };
            Ok(format!(
                "({} {} {})",
                expr_to_string(lhs)?,
                op_str,
                expr_to_string(rhs)?
            ))
        }
        Expr::Call { callee, args } => {
            let arg_strs: Result<Vec<_>, _> = args.iter().map(expr_to_string).collect();
            Ok(format!("{}({})", callee, arg_strs?.join(", ")))
        }
    }
}

fn type_name(ty: &Type) -> String {
    match ty {
        Type::Int => "int".to_string(),
        Type::Void => "void".to_string(),
        Type::Named(name) => name.clone(),
    }
}

/// Errors that can occur during code generation.
#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum CodegenError {
    Unsupported(String),
}

impl std::fmt::Display for CodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CodegenError::Unsupported(msg) => write!(f, "unsupported: {msg}"),
        }
    }
}

impl std::error::Error for CodegenError {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokenize;
    use crate::parser::parse;

    fn compile(src: &str) -> String {
        let tokens = tokenize(src).expect("lex error");
        let program = parse(&tokens).expect("parse error");
        generate(&program).expect("codegen error")
    }

    #[test]
    fn test_empty_function() {
        let out = compile("fn main() {}");
        assert!(out.contains("fn main()"));
        assert!(out.contains("-> void"));
    }

    #[test]
    fn test_function_with_return() {
        let out = compile("fn answer() -> int { return 42; }");
        assert!(out.contains("return 42;"));
    }

    #[test]
    fn test_binary_expr() {
        let out = compile("fn f() -> int { return 1 + 2; }");
        assert!(out.contains("(1 + 2)"));
    }
}
