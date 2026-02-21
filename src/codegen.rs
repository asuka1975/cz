use crate::ast::*;

pub struct CodeGen {
    output: String,
    indent: usize,
    temp_counter: usize,
}

impl CodeGen {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent: 0,
            temp_counter: 0,
        }
    }

    /// Mangle user-defined names to avoid C keyword conflicts.
    /// "main" is kept as-is since it must be the C entry point.
    fn mangle(name: &str) -> String {
        if name == "main" {
            "main".to_string()
        } else {
            format!("cz_{}", name)
        }
    }

    pub fn generate(&mut self, program: &Program) -> String {
        self.emit_line("#include <stdio.h>");
        self.emit_line("#include <stdlib.h>");
        self.emit_line("");

        // Forward declarations
        for func in &program.functions {
            self.emit_forward_decl(func);
        }
        self.emit_line("");

        // print_i32 builtin
        self.emit_line("int cz_print_i32(int x) {");
        self.indent += 1;
        self.emit_line("printf(\"%d\\n\", x);");
        self.emit_line("return 0;");
        self.indent -= 1;
        self.emit_line("}");
        self.emit_line("");

        // Function definitions
        for func in &program.functions {
            self.emit_function(func);
            self.emit_line("");
        }

        self.output.clone()
    }

    fn emit_forward_decl(&mut self, func: &FunctionDef) {
        let c_name = Self::mangle(&func.name);
        let params: Vec<String> = func
            .params
            .iter()
            .map(|p| format!("int {}", Self::mangle(&p.name)))
            .collect();
        self.emit_line(&format!(
            "int {}({});",
            c_name,
            if params.is_empty() {
                "void".to_string()
            } else {
                params.join(", ")
            }
        ));
    }

    fn emit_function(&mut self, func: &FunctionDef) {
        let c_name = Self::mangle(&func.name);
        let params: Vec<String> = func
            .params
            .iter()
            .map(|p| format!("int {}", Self::mangle(&p.name)))
            .collect();
        self.emit_line(&format!(
            "int {}({}) {{",
            c_name,
            if params.is_empty() {
                "void".to_string()
            } else {
                params.join(", ")
            }
        ));
        self.indent += 1;
        self.emit_block_as_return(&func.body);
        self.indent -= 1;
        self.emit_line("}");
    }

    fn emit_block_as_return(&mut self, block: &Block) {
        for stmt in &block.stmts {
            self.emit_stmt(stmt);
        }
        if let Some(expr) = &block.expr {
            let val = self.emit_expr(expr);
            self.emit_line(&format!("return {};", val));
        }
    }

    fn emit_block_value(&mut self, block: &Block) -> String {
        // Use a temp variable to capture the block's value,
        // and a C block scope to allow variable redeclaration.
        let tmp = self.new_temp();
        self.emit_line(&format!("int {};", tmp));
        self.emit_line("{");
        self.indent += 1;
        for stmt in &block.stmts {
            self.emit_stmt(stmt);
        }
        if let Some(expr) = &block.expr {
            let val = self.emit_expr(expr);
            self.emit_line(&format!("{} = {};", tmp, val));
        }
        self.indent -= 1;
        self.emit_line("}");
        tmp
    }

    fn emit_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let { name, init, .. } => {
                let val = self.emit_expr(init);
                self.emit_line(&format!("int {} = {};", Self::mangle(name), val));
            }
            Stmt::While { condition, body } => {
                self.emit_line("while (1) {");
                self.indent += 1;
                let cond = self.emit_expr(condition);
                self.emit_line(&format!("if (!({cond})) break;"));
                // Use a C block scope for the while body
                self.emit_line("{");
                self.indent += 1;
                for s in &body.stmts {
                    self.emit_stmt(s);
                }
                self.indent -= 1;
                self.emit_line("}");
                self.indent -= 1;
                self.emit_line("}");
            }
            Stmt::Return { value, .. } => {
                let val = self.emit_expr(value);
                self.emit_line(&format!("return {};", val));
            }
            Stmt::Expr(expr) => {
                let val = self.emit_expr(expr);
                match expr {
                    Expr::Call { .. } | Expr::Assign { .. } => {}
                    Expr::If {
                        else_block: None, ..
                    } => {}
                    _ => {
                        self.emit_line(&format!("{};", val));
                    }
                }
            }
        }
    }

    fn emit_expr(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::IntegerLiteral(val) => format!("{}", val),
            Expr::BoolLiteral(b) => {
                if *b {
                    "1".to_string()
                } else {
                    "0".to_string()
                }
            }
            Expr::Identifier(name, _) => Self::mangle(name),
            Expr::BinaryOp { op, left, right } => match op {
                BinOp::And => {
                    let tmp = self.new_temp();
                    self.emit_line(&format!("int {};", tmp));
                    let lval = self.emit_expr(left);
                    self.emit_line(&format!("if ({}) {{", lval));
                    self.indent += 1;
                    let rval = self.emit_expr(right);
                    self.emit_line(&format!("{} = ({}) ? 1 : 0;", tmp, rval));
                    self.indent -= 1;
                    self.emit_line("} else {");
                    self.indent += 1;
                    self.emit_line(&format!("{} = 0;", tmp));
                    self.indent -= 1;
                    self.emit_line("}");
                    tmp
                }
                BinOp::Or => {
                    let tmp = self.new_temp();
                    self.emit_line(&format!("int {};", tmp));
                    let lval = self.emit_expr(left);
                    self.emit_line(&format!("if ({}) {{", lval));
                    self.indent += 1;
                    self.emit_line(&format!("{} = {};", tmp, lval));
                    self.indent -= 1;
                    self.emit_line("} else {");
                    self.indent += 1;
                    let rval = self.emit_expr(right);
                    self.emit_line(&format!("{} = {};", tmp, rval));
                    self.indent -= 1;
                    self.emit_line("}");
                    tmp
                }
                BinOp::Div | BinOp::Mod => {
                    let lval = self.emit_expr(left);
                    let rval = self.emit_expr(right);
                    let tmp = self.new_temp();
                    self.emit_line(&format!(
                        "if ({rval} == 0) {{ fprintf(stderr, \"runtime error: division by zero\\n\"); exit(1); }}"
                    ));
                    let c_op = match op {
                        BinOp::Div => "/",
                        BinOp::Mod => "%",
                        _ => unreachable!(),
                    };
                    self.emit_line(&format!("int {} = {} {} {};", tmp, lval, c_op, rval));
                    tmp
                }
                _ => {
                    let lval = self.emit_expr(left);
                    let rval = self.emit_expr(right);
                    let c_op = match op {
                        BinOp::Add => "+",
                        BinOp::Sub => "-",
                        BinOp::Mul => "*",
                        BinOp::Eq => "==",
                        BinOp::NotEq => "!=",
                        BinOp::Lt => "<",
                        BinOp::Gt => ">",
                        BinOp::LtEq => "<=",
                        BinOp::GtEq => ">=",
                        _ => unreachable!(),
                    };
                    format!("({} {} {})", lval, c_op, rval)
                }
            },
            Expr::UnaryOp { op, operand } => {
                let val = self.emit_expr(operand);
                match op {
                    UnaryOp::Neg => format!("(-{})", val),
                    UnaryOp::Not => format!("(!{})", val),
                }
            }
            Expr::Call { name, args, .. } => {
                let arg_strs: Vec<String> = args.iter().map(|a| self.emit_expr(a)).collect();
                let tmp = self.new_temp();
                self.emit_line(&format!(
                    "int {} = {}({});",
                    tmp,
                    Self::mangle(name),
                    arg_strs.join(", ")
                ));
                tmp
            }
            Expr::Assign { name, value, .. } => {
                let val = self.emit_expr(value);
                let c_name = Self::mangle(name);
                self.emit_line(&format!("{} = {};", c_name, val));
                c_name
            }
            Expr::If {
                condition,
                then_block,
                else_block,
            } => match else_block {
                Some(else_clause) => {
                    let tmp = self.new_temp();
                    self.emit_line(&format!("int {};", tmp));
                    let cond = self.emit_expr(condition);
                    self.emit_line(&format!("if ({}) {{", cond));
                    self.indent += 1;
                    let then_val = self.emit_block_value(then_block);
                    self.emit_line(&format!("{} = {};", tmp, then_val));
                    self.indent -= 1;
                    match else_clause.as_ref() {
                        ElseClause::ElseBlock(block) => {
                            self.emit_line("} else {");
                            self.indent += 1;
                            let else_val = self.emit_block_value(block);
                            self.emit_line(&format!("{} = {};", tmp, else_val));
                            self.indent -= 1;
                            self.emit_line("}");
                        }
                        ElseClause::ElseIf(if_expr) => {
                            self.emit_line("} else {");
                            self.indent += 1;
                            let else_val = self.emit_expr(if_expr);
                            self.emit_line(&format!("{} = {};", tmp, else_val));
                            self.indent -= 1;
                            self.emit_line("}");
                        }
                    }
                    tmp
                }
                None => {
                    let cond = self.emit_expr(condition);
                    self.emit_line(&format!("if ({}) {{", cond));
                    self.indent += 1;
                    for s in &then_block.stmts {
                        self.emit_stmt(s);
                    }
                    if let Some(tail) = &then_block.expr {
                        let val = self.emit_expr(tail);
                        // Evaluate but discard (statement context)
                        self.emit_line(&format!("(void){};", val));
                    }
                    self.indent -= 1;
                    self.emit_line("}");
                    "0".to_string()
                }
            },
            Expr::Block(block) => self.emit_block_value(block),
        }
    }

    fn new_temp(&mut self) -> String {
        let name = format!("_t{}", self.temp_counter);
        self.temp_counter += 1;
        name
    }

    fn emit_line(&mut self, s: &str) {
        for _ in 0..self.indent {
            self.output.push_str("    ");
        }
        self.output.push_str(s);
        self.output.push('\n');
    }
}
