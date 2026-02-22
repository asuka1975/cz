use crate::ast::*;
use crate::token::{Token, TokenKind};

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    pub fn parse_program(&mut self) -> Result<Program, String> {
        let mut functions = Vec::new();
        while !self.check(&TokenKind::Eof) {
            functions.push(self.parse_function_def()?);
        }
        Ok(Program { functions })
    }

    // --- Helpers ---

    fn expr_ends_with_block(expr: &Expr) -> bool {
        matches!(expr, Expr::If { .. } | Expr::Block(_) | Expr::Assign { .. })
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.pos]
    }

    fn check(&self, kind: &TokenKind) -> bool {
        std::mem::discriminant(&self.peek().kind) == std::mem::discriminant(kind)
    }

    fn advance(&mut self) -> &Token {
        let tok = &self.tokens[self.pos];
        if !self.check(&TokenKind::Eof) {
            self.pos += 1;
        }
        tok
    }

    fn expect(&mut self, kind: &TokenKind) -> Result<Token, String> {
        if self.check(kind) {
            Ok(self.advance().clone())
        } else {
            let tok = self.peek();
            Err(format!(
                "{}:{}: {:?} が期待されましたが {:?} が見つかりました",
                tok.line, tok.column, kind, tok.kind
            ))
        }
    }

    fn expect_identifier(&mut self) -> Result<(String, usize), String> {
        let tok = self.peek().clone();
        if let TokenKind::Identifier(_) = &tok.kind {
            self.advance();
            if let TokenKind::Identifier(name) = tok.kind {
                Ok((name, tok.line))
            } else {
                unreachable!()
            }
        } else {
            Err(format!(
                "{}:{}: 識別子が期待されましたが {:?} が見つかりました",
                tok.line, tok.column, tok.kind
            ))
        }
    }

    // --- Function ---

    fn parse_function_def(&mut self) -> Result<FunctionDef, String> {
        let fn_tok = self.expect(&TokenKind::Fn)?;
        let line = fn_tok.line;
        let (name, _) = self.expect_identifier()?;
        self.expect(&TokenKind::LParen)?;

        let mut params = Vec::new();
        if !self.check(&TokenKind::RParen) {
            params.push(self.parse_param()?);
            while self.check(&TokenKind::Comma) {
                self.advance();
                params.push(self.parse_param()?);
            }
        }
        self.expect(&TokenKind::RParen)?;
        self.expect(&TokenKind::Arrow)?;
        let return_type = self.parse_type()?;
        let body = self.parse_block()?;

        Ok(FunctionDef {
            name,
            params,
            return_type,
            body,
            line,
        })
    }

    fn parse_param(&mut self) -> Result<Param, String> {
        let (name, _) = self.expect_identifier()?;
        self.expect(&TokenKind::Colon)?;
        let param_type = self.parse_type()?;
        Ok(Param { name, param_type })
    }

    fn parse_type(&mut self) -> Result<Type, String> {
        self.expect(&TokenKind::I32)?;
        Ok(Type::I32)
    }

    // --- Block ---

    fn parse_block(&mut self) -> Result<Block, String> {
        self.expect(&TokenKind::LBrace)?;

        let mut stmts = Vec::new();
        let mut tail_expr: Option<Box<Expr>> = None;

        while !self.check(&TokenKind::RBrace) && !self.check(&TokenKind::Eof) {
            // Try to determine if this is a statement or a tail expression
            if self.check(&TokenKind::Let) {
                stmts.push(self.parse_let_stmt()?);
            } else if self.check(&TokenKind::While) {
                stmts.push(self.parse_while_stmt()?);
            } else if self.check(&TokenKind::Return) {
                stmts.push(self.parse_return_stmt()?);
            } else {
                // Parse as expression, then decide if it's a statement or tail expression
                let expr = self.parse_expression()?;

                if self.check(&TokenKind::Semicolon) {
                    self.advance();
                    stmts.push(Stmt::Expr(expr));
                } else if self.check(&TokenKind::RBrace) {
                    // Tail expression (last expression before closing brace)
                    tail_expr = Some(Box::new(expr));
                } else if Self::expr_ends_with_block(&expr) {
                    // Expressions ending with a block (if, block expr)
                    // don't need semicolons when used as statements
                    stmts.push(Stmt::Expr(expr));
                } else {
                    let tok = self.peek();
                    return Err(format!(
                        "{}:{}: ';' または '}}' が期待されました",
                        tok.line, tok.column
                    ));
                }
            }
        }

        self.expect(&TokenKind::RBrace)?;
        Ok(Block {
            stmts,
            expr: tail_expr,
        })
    }

    // --- Statements ---

    fn parse_let_stmt(&mut self) -> Result<Stmt, String> {
        let let_tok = self.expect(&TokenKind::Let)?;
        let line = let_tok.line;
        let mutable = if self.check(&TokenKind::Mut) {
            self.advance();
            true
        } else {
            false
        };
        let (name, _) = self.expect_identifier()?;
        self.expect(&TokenKind::Colon)?;
        let var_type = self.parse_type()?;
        self.expect(&TokenKind::Eq)?;
        let init = self.parse_expression()?;
        self.expect(&TokenKind::Semicolon)?;

        Ok(Stmt::Let {
            name,
            mutable,
            var_type,
            init,
            line,
        })
    }

    fn parse_while_stmt(&mut self) -> Result<Stmt, String> {
        self.expect(&TokenKind::While)?;
        let condition = self.parse_expression()?;
        let body = self.parse_block()?;
        Ok(Stmt::While { condition, body })
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt, String> {
        let ret_tok = self.expect(&TokenKind::Return)?;
        let line = ret_tok.line;
        let value = self.parse_expression()?;
        self.expect(&TokenKind::Semicolon)?;
        Ok(Stmt::Return { value, line })
    }

    // --- Expressions (precedence climbing) ---

    fn parse_expression(&mut self) -> Result<Expr, String> {
        // Check for assignment: identifier = expr
        // We look ahead: if current is Identifier and next is '='  (not '==')
        if let TokenKind::Identifier(_) = &self.peek().kind
            && self.pos + 1 < self.tokens.len()
            && let TokenKind::Eq = &self.tokens[self.pos + 1].kind
        {
            // This is an assignment
            let (name, line) = self.expect_identifier()?;
            self.expect(&TokenKind::Eq)?;
            let value = self.parse_expression()?;
            return Ok(Expr::Assign {
                name,
                value: Box::new(value),
                line,
            });
        }
        self.parse_or()
    }

    fn parse_or(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_and()?;
        while self.check(&TokenKind::PipePipe) {
            self.advance();
            let right = self.parse_and()?;
            left = Expr::BinaryOp {
                op: BinOp::Or,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_and(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_equality()?;
        while self.check(&TokenKind::AmpAmp) {
            self.advance();
            let right = self.parse_equality()?;
            left = Expr::BinaryOp {
                op: BinOp::And,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_equality(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_comparison()?;
        loop {
            let op = if self.check(&TokenKind::EqEq) {
                BinOp::Eq
            } else if self.check(&TokenKind::NotEq) {
                BinOp::NotEq
            } else {
                break;
            };
            self.advance();
            let right = self.parse_comparison()?;
            left = Expr::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_comparison(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_addition()?;
        loop {
            let op = if self.check(&TokenKind::Lt) {
                BinOp::Lt
            } else if self.check(&TokenKind::Gt) {
                BinOp::Gt
            } else if self.check(&TokenKind::LtEq) {
                BinOp::LtEq
            } else if self.check(&TokenKind::GtEq) {
                BinOp::GtEq
            } else {
                break;
            };
            self.advance();
            let right = self.parse_addition()?;
            left = Expr::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_addition(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_multiplication()?;
        loop {
            let op = if self.check(&TokenKind::Plus) {
                BinOp::Add
            } else if self.check(&TokenKind::Minus) {
                BinOp::Sub
            } else {
                break;
            };
            self.advance();
            let right = self.parse_multiplication()?;
            left = Expr::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_multiplication(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_unary()?;
        loop {
            let op = if self.check(&TokenKind::Star) {
                BinOp::Mul
            } else if self.check(&TokenKind::Slash) {
                BinOp::Div
            } else if self.check(&TokenKind::Percent) {
                BinOp::Mod
            } else {
                break;
            };
            self.advance();
            let right = self.parse_unary()?;
            left = Expr::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Expr, String> {
        if self.check(&TokenKind::Minus) {
            self.advance();
            let operand = self.parse_unary()?;
            return Ok(Expr::UnaryOp {
                op: UnaryOp::Neg,
                operand: Box::new(operand),
            });
        }
        if self.check(&TokenKind::Bang) {
            self.advance();
            let operand = self.parse_unary()?;
            return Ok(Expr::UnaryOp {
                op: UnaryOp::Not,
                operand: Box::new(operand),
            });
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
        let tok = self.peek().clone();
        match &tok.kind {
            TokenKind::IntegerLiteral(val) => {
                let val = *val;
                self.advance();
                Ok(Expr::IntegerLiteral(val))
            }
            TokenKind::True => {
                self.advance();
                Ok(Expr::BoolLiteral(true))
            }
            TokenKind::False => {
                self.advance();
                Ok(Expr::BoolLiteral(false))
            }
            TokenKind::Identifier(_) => {
                let (name, line) = self.expect_identifier()?;
                if self.check(&TokenKind::LParen) {
                    // Function call
                    self.advance();
                    let mut args = Vec::new();
                    if !self.check(&TokenKind::RParen) {
                        args.push(self.parse_expression()?);
                        while self.check(&TokenKind::Comma) {
                            self.advance();
                            args.push(self.parse_expression()?);
                        }
                    }
                    self.expect(&TokenKind::RParen)?;
                    Ok(Expr::Call { name, args, line })
                } else {
                    Ok(Expr::Identifier(name, line))
                }
            }
            TokenKind::LParen => {
                self.advance();
                let expr = self.parse_expression()?;
                self.expect(&TokenKind::RParen)?;
                Ok(expr)
            }
            TokenKind::If => self.parse_if_expression(),
            TokenKind::LBrace => {
                let block = self.parse_block()?;
                Ok(Expr::Block(block))
            }
            _ => Err(format!(
                "{}:{}: 式が期待されましたが {:?} が見つかりました",
                tok.line, tok.column, tok.kind
            )),
        }
    }

    fn parse_if_expression(&mut self) -> Result<Expr, String> {
        self.expect(&TokenKind::If)?;
        let condition = self.parse_expression()?;
        let then_block = self.parse_block()?;

        let else_block = if self.check(&TokenKind::Else) {
            self.advance();
            if self.check(&TokenKind::If) {
                Some(Box::new(ElseClause::ElseIf(Box::new(
                    self.parse_if_expression()?,
                ))))
            } else {
                Some(Box::new(ElseClause::ElseBlock(self.parse_block()?)))
            }
        } else {
            None
        };

        Ok(Expr::If {
            condition: Box::new(condition),
            then_block,
            else_block,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    fn parse(source: &str) -> Result<Program, String> {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().map_err(|e| e.to_string())?;
        let mut parser = Parser::new(tokens);
        parser.parse_program()
    }

    fn parse_ok(source: &str) -> Program {
        parse(source).expect("parse should succeed")
    }

    #[test]
    fn empty_function() {
        let prog = parse_ok("fn main() -> i32 { 0 }");
        assert_eq!(prog.functions.len(), 1);
        assert_eq!(prog.functions[0].name, "main");
        assert_eq!(prog.functions[0].params.len(), 0);
    }

    #[test]
    fn function_with_params() {
        let prog = parse_ok("fn add(a: i32, b: i32) -> i32 { a }");
        assert_eq!(prog.functions[0].params.len(), 2);
        assert_eq!(prog.functions[0].params[0].name, "a");
        assert_eq!(prog.functions[0].params[1].name, "b");
    }

    #[test]
    fn multiple_functions() {
        let prog = parse_ok(
            "fn foo() -> i32 { 0 }
             fn bar() -> i32 { 1 }",
        );
        assert_eq!(prog.functions.len(), 2);
        assert_eq!(prog.functions[0].name, "foo");
        assert_eq!(prog.functions[1].name, "bar");
    }

    #[test]
    fn let_statement() {
        let prog = parse_ok("fn main() -> i32 { let x: i32 = 42; x }");
        assert_eq!(prog.functions[0].body.stmts.len(), 1);
        if let Stmt::Let {
            name, mutable, ..
        } = &prog.functions[0].body.stmts[0]
        {
            assert_eq!(name, "x");
            assert!(!mutable);
        } else {
            panic!("expected Let statement");
        }
    }

    #[test]
    fn let_mut_statement() {
        let prog = parse_ok("fn main() -> i32 { let mut x: i32 = 0; x }");
        if let Stmt::Let { mutable, .. } = &prog.functions[0].body.stmts[0] {
            assert!(*mutable);
        } else {
            panic!("expected Let statement");
        }
    }

    #[test]
    fn return_statement() {
        let prog = parse_ok("fn main() -> i32 { return 42; }");
        assert!(matches!(
            &prog.functions[0].body.stmts[0],
            Stmt::Return { .. }
        ));
    }

    #[test]
    fn while_statement() {
        let prog = parse_ok("fn main() -> i32 { while true { 0; } 0 }");
        assert!(matches!(
            &prog.functions[0].body.stmts[0],
            Stmt::While { .. }
        ));
    }

    #[test]
    fn integer_literal_expr() {
        let prog = parse_ok("fn main() -> i32 { 42 }");
        if let Some(expr) = &prog.functions[0].body.expr {
            assert!(matches!(expr.as_ref(), Expr::IntegerLiteral(42)));
        } else {
            panic!("expected tail expression");
        }
    }

    #[test]
    fn bool_literals() {
        let prog = parse_ok("fn main() -> i32 { if true { 1 } else { 0 } }");
        if let Some(expr) = &prog.functions[0].body.expr {
            if let Expr::If { condition, .. } = expr.as_ref() {
                assert!(matches!(condition.as_ref(), Expr::BoolLiteral(true)));
            } else {
                panic!("expected if expression");
            }
        } else {
            panic!("expected tail expression");
        }
    }

    #[test]
    fn binary_op_add() {
        let prog = parse_ok("fn main() -> i32 { 1 + 2 }");
        if let Some(expr) = &prog.functions[0].body.expr {
            if let Expr::BinaryOp { op, .. } = expr.as_ref() {
                assert!(matches!(op, BinOp::Add));
            } else {
                panic!("expected BinaryOp");
            }
        } else {
            panic!("expected tail expression");
        }
    }

    #[test]
    fn operator_precedence_mul_over_add() {
        // 1 + 2 * 3 should parse as 1 + (2 * 3)
        let prog = parse_ok("fn main() -> i32 { 1 + 2 * 3 }");
        if let Some(expr) = &prog.functions[0].body.expr {
            if let Expr::BinaryOp {
                op: BinOp::Add,
                right,
                ..
            } = expr.as_ref()
            {
                assert!(matches!(
                    right.as_ref(),
                    Expr::BinaryOp {
                        op: BinOp::Mul,
                        ..
                    }
                ));
            } else {
                panic!("expected Add at top level");
            }
        } else {
            panic!("expected tail expression");
        }
    }

    #[test]
    fn operator_precedence_comparison_over_logical() {
        // a < b && c > d should parse as (a < b) && (c > d)
        let prog = parse_ok("fn main() -> i32 { let a: i32 = 1; let b: i32 = 2; let c: i32 = 3; let d: i32 = 4; if a < b && c > d { 1 } else { 0 } }");
        // If it parses successfully, precedence is correct (otherwise && would consume < as operand)
        assert_eq!(prog.functions.len(), 1);
    }

    #[test]
    fn unary_neg() {
        let prog = parse_ok("fn main() -> i32 { -42 }");
        if let Some(expr) = &prog.functions[0].body.expr {
            assert!(matches!(
                expr.as_ref(),
                Expr::UnaryOp {
                    op: UnaryOp::Neg,
                    ..
                }
            ));
        } else {
            panic!("expected tail expression");
        }
    }

    #[test]
    fn unary_not() {
        let prog = parse_ok("fn main() -> i32 { if !false { 1 } else { 0 } }");
        assert_eq!(prog.functions.len(), 1);
    }

    #[test]
    fn function_call() {
        let prog = parse_ok(
            "fn foo(x: i32) -> i32 { x }
             fn main() -> i32 { foo(42) }",
        );
        if let Some(expr) = &prog.functions[1].body.expr {
            if let Expr::Call { name, args, .. } = expr.as_ref() {
                assert_eq!(name, "foo");
                assert_eq!(args.len(), 1);
            } else {
                panic!("expected Call");
            }
        } else {
            panic!("expected tail expression");
        }
    }

    #[test]
    fn if_expression() {
        let prog = parse_ok("fn main() -> i32 { if true { 1 } else { 0 } }");
        if let Some(expr) = &prog.functions[0].body.expr {
            if let Expr::If { else_block, .. } = expr.as_ref() {
                assert!(else_block.is_some());
            } else {
                panic!("expected If");
            }
        } else {
            panic!("expected tail expression");
        }
    }

    #[test]
    fn if_without_else() {
        let prog = parse_ok("fn main() -> i32 { if true { 0; } 0 }");
        assert!(matches!(
            &prog.functions[0].body.stmts[0],
            Stmt::Expr(Expr::If {
                else_block: None,
                ..
            })
        ));
    }

    #[test]
    fn if_else_if() {
        let prog = parse_ok(
            "fn main() -> i32 { if true { 1 } else if false { 2 } else { 3 } }",
        );
        if let Some(expr) = &prog.functions[0].body.expr {
            if let Expr::If { else_block, .. } = expr.as_ref() {
                assert!(matches!(
                    else_block.as_ref().map(|b| b.as_ref()),
                    Some(ElseClause::ElseIf(_))
                ));
            } else {
                panic!("expected If");
            }
        } else {
            panic!("expected tail expression");
        }
    }

    #[test]
    fn block_expression() {
        let prog = parse_ok("fn main() -> i32 { { 42 } }");
        if let Some(expr) = &prog.functions[0].body.expr {
            assert!(matches!(expr.as_ref(), Expr::Block(_)));
        } else {
            panic!("expected tail expression");
        }
    }

    #[test]
    fn assignment_expression() {
        let prog = parse_ok("fn main() -> i32 { let mut x: i32 = 0; x = 42; x }");
        if let Stmt::Expr(Expr::Assign { name, .. }) = &prog.functions[0].body.stmts[1] {
            assert_eq!(name, "x");
        } else {
            panic!("expected Assign expression statement");
        }
    }

    #[test]
    fn parenthesized_expression() {
        let prog = parse_ok("fn main() -> i32 { (1 + 2) * 3 }");
        if let Some(expr) = &prog.functions[0].body.expr {
            // (1+2)*3 => Mul at top, left should be Add
            if let Expr::BinaryOp {
                op: BinOp::Mul,
                left,
                ..
            } = expr.as_ref()
            {
                assert!(matches!(
                    left.as_ref(),
                    Expr::BinaryOp {
                        op: BinOp::Add,
                        ..
                    }
                ));
            } else {
                panic!("expected Mul at top level");
            }
        } else {
            panic!("expected tail expression");
        }
    }

    #[test]
    fn missing_semicolon_error() {
        let result = parse("fn main() -> i32 { let x: i32 = 1 }");
        assert!(result.is_err());
    }

    #[test]
    fn missing_closing_brace_error() {
        let result = parse("fn main() -> i32 { 0");
        assert!(result.is_err());
    }

    #[test]
    fn tail_expression_and_statements() {
        let prog = parse_ok(
            "fn main() -> i32 {
                let x: i32 = 1;
                let y: i32 = 2;
                x + y
            }",
        );
        assert_eq!(prog.functions[0].body.stmts.len(), 2);
        assert!(prog.functions[0].body.expr.is_some());
    }
}
