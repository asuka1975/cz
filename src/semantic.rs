use crate::ast::*;
use std::collections::HashMap;

struct VarInfo {
    mutable: bool,
}

struct Scope {
    vars: HashMap<String, VarInfo>,
}

struct FuncInfo {
    param_count: usize,
}

pub struct SemanticAnalyzer {
    scopes: Vec<Scope>,
    functions: HashMap<String, FuncInfo>,
    errors: Vec<String>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        let mut functions = HashMap::new();
        functions.insert("print_i32".to_string(), FuncInfo { param_count: 1 });
        Self {
            scopes: Vec::new(),
            functions,
            errors: Vec::new(),
        }
    }

    pub fn analyze(&mut self, program: &Program) -> Result<(), Vec<String>> {
        // First pass: register all functions (forward reference support)
        for func in &program.functions {
            if self.functions.contains_key(&func.name) {
                self.errors.push(format!(
                    "{}行目: 関数 '{}' は既に定義されています",
                    func.line, func.name
                ));
            } else {
                self.functions.insert(
                    func.name.clone(),
                    FuncInfo {
                        param_count: func.params.len(),
                    },
                );
            }
        }

        // Second pass: analyze function bodies
        for func in &program.functions {
            self.analyze_function(func);
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    fn analyze_function(&mut self, func: &FunctionDef) {
        self.push_scope();
        for param in &func.params {
            self.define_var(&param.name, false, func.line);
        }
        let has_value = self.analyze_block(&func.body);

        if !has_value && !self.block_always_returns(&func.body) {
            self.errors.push(format!(
                "{}行目: 関数 '{}' は i32 の戻り値が必要ですが、末尾式がありません",
                func.line, func.name
            ));
        }

        self.pop_scope();
    }

    fn block_always_returns(&self, block: &Block) -> bool {
        for stmt in &block.stmts {
            if let Stmt::Return { .. } = stmt {
                return true;
            }
            if let Stmt::Expr(Expr::If {
                then_block,
                else_block: Some(else_clause),
                ..
            }) = stmt
            {
                let then_returns = self.block_always_returns(then_block);
                let else_returns = match else_clause.as_ref() {
                    ElseClause::ElseBlock(b) => self.block_always_returns(b),
                    ElseClause::ElseIf(expr) => self.expr_always_returns(expr),
                };
                if then_returns && else_returns {
                    return true;
                }
            }
        }
        false
    }

    fn expr_always_returns(&self, expr: &Expr) -> bool {
        if let Expr::If {
            then_block,
            else_block: Some(else_clause),
            ..
        } = expr
        {
            let then_returns = self.block_always_returns(then_block);
            let else_returns = match else_clause.as_ref() {
                ElseClause::ElseBlock(b) => self.block_always_returns(b),
                ElseClause::ElseIf(e) => self.expr_always_returns(e),
            };
            then_returns && else_returns
        } else {
            false
        }
    }

    /// Returns true if the block has a tail expression (i.e. produces a value).
    fn analyze_block(&mut self, block: &Block) -> bool {
        for stmt in &block.stmts {
            self.analyze_stmt(stmt);
        }
        if let Some(expr) = &block.expr {
            self.analyze_expr_value(expr);
            true
        } else {
            false
        }
    }

    fn analyze_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let {
                name,
                mutable,
                init,
                line,
                ..
            } => {
                self.analyze_expr_value(init);
                self.define_var(name, *mutable, *line);
            }
            Stmt::While { condition, body } => {
                self.analyze_expr(condition);
                self.push_scope();
                self.analyze_block(body);
                self.pop_scope();
            }
            Stmt::Return { value, .. } => {
                self.analyze_expr_value(value);
            }
            Stmt::Expr(expr) => {
                self.analyze_expr(expr);
            }
        }
    }

    /// Analyze an expression that is used in a value context
    /// (e.g., let init, return value, block tail expression, function argument).
    /// This checks that if-expressions have both branches with values.
    fn analyze_expr_value(&mut self, expr: &Expr) {
        self.analyze_expr(expr);
        self.check_expr_has_value(expr);
    }

    fn check_expr_has_value(&mut self, expr: &Expr) {
        match expr {
            Expr::If {
                then_block,
                else_block,
                ..
            } => {
                match else_block {
                    None => {
                        self.errors
                            .push("if 式を値として使用するには else 分岐が必要です".to_string());
                    }
                    Some(else_clause) => {
                        // Check then block has value
                        if then_block.expr.is_none() && !self.block_always_returns(then_block) {
                            self.errors
                                .push("if 式の then 分岐に値がありません".to_string());
                        }
                        // Check else block has value
                        match else_clause.as_ref() {
                            ElseClause::ElseBlock(block) => {
                                if block.expr.is_none() && !self.block_always_returns(block) {
                                    self.errors
                                        .push("if 式の else 分岐に値がありません".to_string());
                                }
                            }
                            ElseClause::ElseIf(if_expr) => {
                                self.check_expr_has_value(if_expr);
                            }
                        }
                    }
                }
            }
            Expr::Block(block) => {
                if block.expr.is_none() && !self.block_always_returns(block) {
                    self.errors.push("ブロック式に値がありません".to_string());
                }
            }
            _ => {}
        }
    }

    fn analyze_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::IntegerLiteral(_) | Expr::BoolLiteral(_) => {}
            Expr::Identifier(name, line) => {
                if !self.lookup_var(name) {
                    self.errors
                        .push(format!("{}行目: 未定義の変数 '{}'", line, name));
                }
            }
            Expr::BinaryOp { left, right, .. } => {
                self.analyze_expr(left);
                self.analyze_expr(right);
            }
            Expr::UnaryOp { operand, .. } => {
                self.analyze_expr(operand);
            }
            Expr::Call { name, args, line } => {
                for arg in args {
                    self.analyze_expr_value(arg);
                }
                if let Some(func_info) = self.functions.get(name) {
                    if args.len() != func_info.param_count {
                        self.errors.push(format!(
                            "{}行目: 関数 '{}' は {} 個の引数を取りますが {} 個渡されました",
                            line,
                            name,
                            func_info.param_count,
                            args.len()
                        ));
                    }
                } else {
                    self.errors
                        .push(format!("{}行目: 未定義の関数 '{}'", line, name));
                }
            }
            Expr::Assign { name, value, line } => {
                self.analyze_expr_value(value);
                if let Some(info) = self.find_var(name) {
                    if !info.mutable {
                        self.errors.push(format!(
                            "{}行目: イミュータブル変数 '{}' への再代入はできません",
                            line, name
                        ));
                    }
                } else {
                    self.errors
                        .push(format!("{}行目: 未定義の変数 '{}'", line, name));
                }
            }
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                self.analyze_expr(condition);
                self.push_scope();
                self.analyze_block(then_block);
                self.pop_scope();
                if let Some(else_clause) = else_block {
                    match else_clause.as_ref() {
                        ElseClause::ElseBlock(block) => {
                            self.push_scope();
                            self.analyze_block(block);
                            self.pop_scope();
                        }
                        ElseClause::ElseIf(expr) => {
                            self.analyze_expr(expr);
                        }
                    }
                }
            }
            Expr::Block(block) => {
                self.push_scope();
                self.analyze_block(block);
                self.pop_scope();
            }
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope {
            vars: HashMap::new(),
        });
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn define_var(&mut self, name: &str, mutable: bool, line: usize) {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.vars.contains_key(name) {
                self.errors.push(format!(
                    "{}行目: 変数 '{}' は同一スコープ内で既に宣言されています",
                    line, name
                ));
            } else {
                scope.vars.insert(name.to_string(), VarInfo { mutable });
            }
        }
    }

    fn lookup_var(&self, name: &str) -> bool {
        for scope in self.scopes.iter().rev() {
            if scope.vars.contains_key(name) {
                return true;
            }
        }
        false
    }

    fn find_var(&self, name: &str) -> Option<&VarInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.vars.get(name) {
                return Some(info);
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn analyze(source: &str) -> Result<(), Vec<String>> {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse_program().unwrap();
        let mut analyzer = SemanticAnalyzer::new();
        analyzer.analyze(&program)
    }

    fn analyze_errors(source: &str) -> Vec<String> {
        analyze(source).unwrap_err()
    }

    #[test]
    fn valid_simple_function() {
        assert!(analyze("fn main() -> i32 { 0 }").is_ok());
    }

    #[test]
    fn valid_function_with_params() {
        assert!(analyze("fn add(a: i32, b: i32) -> i32 { a + b }").is_ok());
    }

    #[test]
    fn valid_let_and_use() {
        assert!(analyze("fn main() -> i32 { let x: i32 = 42; x }").is_ok());
    }

    #[test]
    fn valid_mutable_reassign() {
        assert!(
            analyze("fn main() -> i32 { let mut x: i32 = 0; x = 42; x }").is_ok()
        );
    }

    #[test]
    fn valid_forward_reference() {
        assert!(analyze(
            "fn main() -> i32 { foo() }
             fn foo() -> i32 { 42 }"
        )
        .is_ok());
    }

    #[test]
    fn valid_print_i32_builtin() {
        assert!(analyze("fn main() -> i32 { print_i32(42) }").is_ok());
    }

    #[test]
    fn undefined_variable() {
        let errors = analyze_errors("fn main() -> i32 { x }");
        assert!(errors.iter().any(|e| e.contains("未定義の変数 'x'")));
    }

    #[test]
    fn undefined_function() {
        let errors = analyze_errors("fn main() -> i32 { foo() }");
        assert!(errors.iter().any(|e| e.contains("未定義の関数 'foo'")));
    }

    #[test]
    fn argument_count_mismatch() {
        let errors = analyze_errors(
            "fn foo(a: i32) -> i32 { a }
             fn main() -> i32 { foo(1, 2) }",
        );
        assert!(errors
            .iter()
            .any(|e| e.contains("1 個の引数を取りますが 2 個渡されました")));
    }

    #[test]
    fn immutable_reassign() {
        let errors = analyze_errors("fn main() -> i32 { let x: i32 = 0; x = 1; x }");
        assert!(errors
            .iter()
            .any(|e| e.contains("イミュータブル変数 'x' への再代入")));
    }

    #[test]
    fn duplicate_function() {
        let errors = analyze_errors(
            "fn foo() -> i32 { 0 }
             fn foo() -> i32 { 1 }
             fn main() -> i32 { 0 }",
        );
        assert!(errors
            .iter()
            .any(|e| e.contains("関数 'foo' は既に定義されています")));
    }

    #[test]
    fn variable_scope_inner() {
        // Variable defined in inner scope should not be visible in outer scope
        let errors = analyze_errors(
            "fn main() -> i32 {
                if true { let y: i32 = 1; y; }
                y
            }",
        );
        assert!(errors.iter().any(|e| e.contains("未定義の変数 'y'")));
    }

    #[test]
    fn missing_tail_expression() {
        let errors = analyze_errors("fn main() -> i32 { let x: i32 = 0; }");
        assert!(errors.iter().any(|e| e.contains("末尾式がありません")));
    }

    #[test]
    fn if_value_requires_else() {
        let errors = analyze_errors("fn main() -> i32 { if true { 1 } }");
        assert!(errors
            .iter()
            .any(|e| e.contains("else 分岐が必要です")));
    }

    #[test]
    fn valid_while_loop() {
        assert!(analyze(
            "fn main() -> i32 {
                let mut x: i32 = 0;
                while x < 10 {
                    x = x + 1;
                }
                x
            }"
        )
        .is_ok());
    }

    #[test]
    fn valid_return_statement() {
        assert!(analyze("fn main() -> i32 { return 42; }").is_ok());
    }

    #[test]
    fn valid_if_else_as_value() {
        assert!(
            analyze("fn main() -> i32 { if true { 1 } else { 0 } }").is_ok()
        );
    }

    #[test]
    fn duplicate_var_in_same_scope() {
        let errors = analyze_errors(
            "fn main() -> i32 { let x: i32 = 1; let x: i32 = 2; x }",
        );
        assert!(errors
            .iter()
            .any(|e| e.contains("変数 'x' は同一スコープ内で既に宣言されています")));
    }
}
