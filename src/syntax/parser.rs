use crate::arena::Arena;
use crate::diagnostics::Span;
use crate::syntax::ast::*;
use crate::syntax::token::{Token, TokenKind};

pub struct ParseResult {
    pub program: Program,
    pub expr_arena: Arena<Expr>,
    pub stmt_arena: Arena<Stmt>,
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    expr_arena: Arena<Expr>,
    stmt_arena: Arena<Stmt>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: 0,
            expr_arena: Arena::new(),
            stmt_arena: Arena::new(),
        }
    }

    pub fn parse_program(mut self) -> Result<ParseResult, String> {
        let mut functions = Vec::new();
        let mut structs = Vec::new();
        let mut enums = Vec::new();
        while !self.check(&TokenKind::Eof) {
            if self.check(&TokenKind::Fn) {
                functions.push(self.parse_function_def()?);
            } else if self.check(&TokenKind::Struct) {
                structs.push(self.parse_struct_def()?);
            } else if self.check(&TokenKind::Enum) {
                enums.push(self.parse_enum_def()?);
            } else {
                let tok = self.peek();
                return Err(format!(
                    "{}:{}: トップレベルで fn, struct, enum が期待されましたが {:?} が見つかりました",
                    self.span_to_line(tok.span),
                    self.span_to_col(tok.span),
                    tok.kind
                ));
            }
        }
        Ok(ParseResult {
            program: Program {
                functions,
                structs,
                enums,
            },
            expr_arena: self.expr_arena,
            stmt_arena: self.stmt_arena,
        })
    }

    // --- Arena helpers ---

    fn alloc_expr(&mut self, expr: Expr) -> ExprId {
        self.expr_arena.alloc(expr)
    }

    fn alloc_stmt(&mut self, stmt: Stmt) -> StmtId {
        self.stmt_arena.alloc(stmt)
    }

    // --- Span helpers (for error message backward compatibility) ---

    fn span_to_line(&self, span: Span) -> usize {
        let offset = span.start as usize;
        // Reconstruct the source from tokens is not possible, so we search tokens
        // for line info. Instead, we'll count newlines in source prefix.
        // Since we don't have the source, we use token positions.
        // Actually, we need to compute from token spans. Let's keep a simpler approach:
        // scan earlier tokens to find the line.
        // For now, use a simple approach: find the token at this span and use stored info.
        // Since we track line/column in the lexer but not in tokens anymore,
        // we need to compute from byte offset.
        // We don't have the source here, so we'll use a rough estimate.
        // The real error rendering happens in Diagnostics::render().
        // For parser errors, we just report the byte offset.
        offset + 1 // placeholder - actual line number computed elsewhere
    }

    fn span_to_col(&self, _span: Span) -> usize {
        1 // placeholder
    }

    // We need line:column for backward-compatible error messages.
    // The simplest approach: store line/col alongside span in Token.
    // But our spec says Token only has span. Let's compute from source later.
    // For now, parser errors use a different format helper.

    fn err_at_current(&self, msg: &str) -> String {
        let tok = self.peek();
        // Use the token's span start as a rough identifier
        format!("構文解析エラー (offset {}): {}", tok.span.start, msg)
    }

    fn err_expected(&self, expected: &str) -> String {
        let tok = self.peek();
        format!(
            "構文解析エラー (offset {}): {} が期待されましたが {:?} が見つかりました",
            tok.span.start, expected, tok.kind
        )
    }

    // --- Helpers ---

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
            Err(self.err_expected(&format!("{:?}", kind)))
        }
    }

    fn expect_identifier(&mut self) -> Result<(String, Span), String> {
        let tok = self.peek().clone();
        if let TokenKind::Identifier(_) = &tok.kind {
            self.advance();
            if let TokenKind::Identifier(name) = tok.kind {
                Ok((name, tok.span))
            } else {
                unreachable!()
            }
        } else {
            Err(self.err_expected("識別子"))
        }
    }

    // --- Struct/Enum Definitions ---

    fn parse_struct_def(&mut self) -> Result<StructDef, String> {
        let struct_tok = self.expect(&TokenKind::Struct)?;
        let start = struct_tok.span.start;
        let (name, _) = self.expect_identifier()?;
        self.expect(&TokenKind::LBrace)?;
        let mut fields = Vec::new();
        while !self.check(&TokenKind::RBrace) && !self.check(&TokenKind::Eof) {
            let (field_name, _) = self.expect_identifier()?;
            self.expect(&TokenKind::Colon)?;
            let field_type = self.parse_type()?;
            fields.push(FieldDef {
                name: field_name,
                field_type,
            });
            if !self.check(&TokenKind::RBrace) && self.check(&TokenKind::Comma) {
                self.advance();
            }
        }
        let end_tok = self.expect(&TokenKind::RBrace)?;
        Ok(StructDef {
            name,
            fields,
            span: Span::new(start as usize, end_tok.span.end as usize),
        })
    }

    fn parse_enum_def(&mut self) -> Result<EnumDef, String> {
        let enum_tok = self.expect(&TokenKind::Enum)?;
        let start = enum_tok.span.start;
        let (name, _) = self.expect_identifier()?;
        self.expect(&TokenKind::LBrace)?;
        let mut variants = Vec::new();
        while !self.check(&TokenKind::RBrace) && !self.check(&TokenKind::Eof) {
            let (variant_name, _) = self.expect_identifier()?;
            let kind = if self.check(&TokenKind::LParen) {
                self.advance();
                let mut types = Vec::new();
                if !self.check(&TokenKind::RParen) {
                    types.push(self.parse_type()?);
                    while self.check(&TokenKind::Comma) {
                        self.advance();
                        if !self.check(&TokenKind::RParen) {
                            types.push(self.parse_type()?);
                        }
                    }
                }
                self.expect(&TokenKind::RParen)?;
                VariantKind::Tuple(types)
            } else if self.check(&TokenKind::LBrace) {
                self.advance();
                let mut fields = Vec::new();
                while !self.check(&TokenKind::RBrace) && !self.check(&TokenKind::Eof) {
                    let (field_name, _) = self.expect_identifier()?;
                    self.expect(&TokenKind::Colon)?;
                    let field_type = self.parse_type()?;
                    fields.push(FieldDef {
                        name: field_name,
                        field_type,
                    });
                    if !self.check(&TokenKind::RBrace) && self.check(&TokenKind::Comma) {
                        self.advance();
                    }
                }
                self.expect(&TokenKind::RBrace)?;
                VariantKind::Struct(fields)
            } else {
                VariantKind::Unit
            };
            variants.push(VariantDef {
                name: variant_name,
                kind,
            });
            if !self.check(&TokenKind::RBrace) && self.check(&TokenKind::Comma) {
                self.advance();
            }
        }
        let end_tok = self.expect(&TokenKind::RBrace)?;
        Ok(EnumDef {
            name,
            variants,
            span: Span::new(start as usize, end_tok.span.end as usize),
        })
    }

    // --- Function ---

    fn parse_function_def(&mut self) -> Result<FunctionDef, String> {
        let fn_tok = self.expect(&TokenKind::Fn)?;
        let start = fn_tok.span.start;
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

        let return_type = if self.check(&TokenKind::Arrow) {
            self.advance();
            self.parse_type()?
        } else {
            Type::Unit
        };

        let body = self.parse_block()?;
        let end = self.tokens[self.pos.saturating_sub(1)].span.end;

        Ok(FunctionDef {
            name,
            params,
            return_type,
            body,
            span: Span::new(start as usize, end as usize),
        })
    }

    fn parse_param(&mut self) -> Result<Param, String> {
        let (name, _) = self.expect_identifier()?;
        self.expect(&TokenKind::Colon)?;
        let param_type = self.parse_type()?;
        Ok(Param { name, param_type })
    }

    fn parse_type(&mut self) -> Result<Type, String> {
        let tok = self.peek().clone();
        match &tok.kind {
            TokenKind::I8 => {
                self.advance();
                Ok(Type::I8)
            }
            TokenKind::I16 => {
                self.advance();
                Ok(Type::I16)
            }
            TokenKind::I32 => {
                self.advance();
                Ok(Type::I32)
            }
            TokenKind::I64 => {
                self.advance();
                Ok(Type::I64)
            }
            TokenKind::F32 => {
                self.advance();
                Ok(Type::F32)
            }
            TokenKind::F64 => {
                self.advance();
                Ok(Type::F64)
            }
            TokenKind::Bool => {
                self.advance();
                Ok(Type::Bool)
            }
            TokenKind::LParen => {
                self.advance();
                if self.check(&TokenKind::RParen) {
                    self.advance();
                    return Ok(Type::Unit);
                }
                let mut types = vec![self.parse_type()?];
                while self.check(&TokenKind::Comma) {
                    self.advance();
                    if !self.check(&TokenKind::RParen) {
                        types.push(self.parse_type()?);
                    }
                }
                self.expect(&TokenKind::RParen)?;
                if types.len() == 1 {
                    Ok(types.into_iter().next().unwrap())
                } else {
                    Ok(Type::Tuple(types))
                }
            }
            TokenKind::Identifier(_) => {
                let (name, _) = self.expect_identifier()?;
                Ok(Type::Named(name))
            }
            _ => Err(self.err_expected("型")),
        }
    }

    // --- Block ---

    fn parse_block(&mut self) -> Result<Block, String> {
        self.expect(&TokenKind::LBrace)?;

        let mut stmts = Vec::new();
        let mut tail_expr: Option<ExprId> = None;

        while !self.check(&TokenKind::RBrace) && !self.check(&TokenKind::Eof) {
            if self.check(&TokenKind::Let) {
                stmts.push(self.parse_let_stmt()?);
            } else if self.check(&TokenKind::Return) {
                stmts.push(self.parse_return_stmt()?);
            } else if self.check(&TokenKind::Break) {
                stmts.push(self.parse_break_stmt()?);
            } else if self.check(&TokenKind::Continue) {
                stmts.push(self.parse_continue_stmt()?);
            } else {
                let expr_id = self.parse_expression()?;

                if self.check(&TokenKind::Semicolon) {
                    self.advance();
                    let sid = self.alloc_stmt(Stmt::Expr(expr_id));
                    stmts.push(sid);
                } else if self.check(&TokenKind::RBrace) {
                    tail_expr = Some(expr_id);
                } else if self.expr_arena.get(expr_id).ends_with_block() {
                    let sid = self.alloc_stmt(Stmt::Expr(expr_id));
                    stmts.push(sid);
                } else {
                    return Err(self.err_at_current("';' または '}' が期待されました"));
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

    fn parse_let_stmt(&mut self) -> Result<StmtId, String> {
        let let_tok = self.expect(&TokenKind::Let)?;
        let start = let_tok.span.start;
        let mutable = if self.check(&TokenKind::Mut) {
            self.advance();
            true
        } else {
            false
        };
        let (name, _) = self.expect_identifier()?;

        let var_type = if self.check(&TokenKind::Colon) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(&TokenKind::Eq)?;
        let init = self.parse_expression()?;
        let semi = self.expect(&TokenKind::Semicolon)?;

        Ok(self.alloc_stmt(Stmt::Let {
            name,
            mutable,
            var_type,
            init,
            span: Span::new(start as usize, semi.span.end as usize),
        }))
    }

    fn parse_return_stmt(&mut self) -> Result<StmtId, String> {
        let ret_tok = self.expect(&TokenKind::Return)?;
        let start = ret_tok.span.start;
        if self.check(&TokenKind::Semicolon) {
            let semi = self.advance().clone();
            Ok(self.alloc_stmt(Stmt::Return {
                value: None,
                span: Span::new(start as usize, semi.span.end as usize),
            }))
        } else {
            let value = self.parse_expression()?;
            let semi = self.expect(&TokenKind::Semicolon)?;
            Ok(self.alloc_stmt(Stmt::Return {
                value: Some(value),
                span: Span::new(start as usize, semi.span.end as usize),
            }))
        }
    }

    fn parse_break_stmt(&mut self) -> Result<StmtId, String> {
        let break_tok = self.expect(&TokenKind::Break)?;
        let start = break_tok.span.start;
        let label = if let TokenKind::Label(_) = &self.peek().kind {
            let tok = self.advance().clone();
            if let TokenKind::Label(name) = tok.kind {
                Some(name)
            } else {
                unreachable!()
            }
        } else {
            None
        };
        let value = if !self.check(&TokenKind::Semicolon) {
            Some(self.parse_expression()?)
        } else {
            None
        };
        let semi = self.expect(&TokenKind::Semicolon)?;
        Ok(self.alloc_stmt(Stmt::Break {
            label,
            value,
            span: Span::new(start as usize, semi.span.end as usize),
        }))
    }

    fn parse_continue_stmt(&mut self) -> Result<StmtId, String> {
        let cont_tok = self.expect(&TokenKind::Continue)?;
        let start = cont_tok.span.start;
        let label = if let TokenKind::Label(_) = &self.peek().kind {
            let tok = self.advance().clone();
            if let TokenKind::Label(name) = tok.kind {
                Some(name)
            } else {
                unreachable!()
            }
        } else {
            None
        };
        let semi = self.expect(&TokenKind::Semicolon)?;
        Ok(self.alloc_stmt(Stmt::Continue {
            label,
            span: Span::new(start as usize, semi.span.end as usize),
        }))
    }

    // --- Expressions (precedence climbing) ---

    fn parse_expression(&mut self) -> Result<ExprId, String> {
        // Check for assignment: identifier = expr
        if let TokenKind::Identifier(_) = &self.peek().kind
            && self.pos + 1 < self.tokens.len()
            && matches!(&self.tokens[self.pos + 1].kind, TokenKind::Eq)
        {
            let (name, name_span) = self.expect_identifier()?;
            self.expect(&TokenKind::Eq)?;
            let value = self.parse_expression()?;
            let end = self.expr_arena.get(value).span().end;
            return Ok(self.alloc_expr(Expr::Assign {
                name,
                value,
                span: Span::new(name_span.start as usize, end as usize),
            }));
        }
        self.parse_or()
    }

    fn parse_or(&mut self) -> Result<ExprId, String> {
        let mut left = self.parse_and()?;
        while self.check(&TokenKind::PipePipe) {
            self.advance();
            let right = self.parse_and()?;
            let span = Span::new(
                self.expr_arena.get(left).span().start as usize,
                self.expr_arena.get(right).span().end as usize,
            );
            left = self.alloc_expr(Expr::BinaryOp {
                op: BinOp::Or,
                left,
                right,
                span,
            });
        }
        Ok(left)
    }

    fn parse_and(&mut self) -> Result<ExprId, String> {
        let mut left = self.parse_equality()?;
        while self.check(&TokenKind::AmpAmp) {
            self.advance();
            let right = self.parse_equality()?;
            let span = Span::new(
                self.expr_arena.get(left).span().start as usize,
                self.expr_arena.get(right).span().end as usize,
            );
            left = self.alloc_expr(Expr::BinaryOp {
                op: BinOp::And,
                left,
                right,
                span,
            });
        }
        Ok(left)
    }

    fn parse_equality(&mut self) -> Result<ExprId, String> {
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
            let span = Span::new(
                self.expr_arena.get(left).span().start as usize,
                self.expr_arena.get(right).span().end as usize,
            );
            left = self.alloc_expr(Expr::BinaryOp {
                op,
                left,
                right,
                span,
            });
        }
        Ok(left)
    }

    fn parse_comparison(&mut self) -> Result<ExprId, String> {
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
            let span = Span::new(
                self.expr_arena.get(left).span().start as usize,
                self.expr_arena.get(right).span().end as usize,
            );
            left = self.alloc_expr(Expr::BinaryOp {
                op,
                left,
                right,
                span,
            });
        }
        Ok(left)
    }

    fn parse_addition(&mut self) -> Result<ExprId, String> {
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
            let span = Span::new(
                self.expr_arena.get(left).span().start as usize,
                self.expr_arena.get(right).span().end as usize,
            );
            left = self.alloc_expr(Expr::BinaryOp {
                op,
                left,
                right,
                span,
            });
        }
        Ok(left)
    }

    fn parse_multiplication(&mut self) -> Result<ExprId, String> {
        let mut left = self.parse_cast()?;
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
            let right = self.parse_cast()?;
            let span = Span::new(
                self.expr_arena.get(left).span().start as usize,
                self.expr_arena.get(right).span().end as usize,
            );
            left = self.alloc_expr(Expr::BinaryOp {
                op,
                left,
                right,
                span,
            });
        }
        Ok(left)
    }

    fn parse_cast(&mut self) -> Result<ExprId, String> {
        let mut expr = self.parse_unary()?;
        while self.check(&TokenKind::As) {
            self.advance();
            let target_type = self.parse_type()?;
            let start = self.expr_arena.get(expr).span().start;
            let end = self.tokens[self.pos.saturating_sub(1)].span.end;
            expr = self.alloc_expr(Expr::Cast {
                expr,
                target_type,
                span: Span::new(start as usize, end as usize),
            });
        }
        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<ExprId, String> {
        if self.check(&TokenKind::Minus) {
            let op_tok = self.advance().clone();
            let operand = self.parse_unary()?;
            let end = self.expr_arena.get(operand).span().end;
            return Ok(self.alloc_expr(Expr::UnaryOp {
                op: UnaryOp::Neg,
                operand,
                span: Span::new(op_tok.span.start as usize, end as usize),
            }));
        }
        if self.check(&TokenKind::Bang) {
            let op_tok = self.advance().clone();
            let operand = self.parse_unary()?;
            let end = self.expr_arena.get(operand).span().end;
            return Ok(self.alloc_expr(Expr::UnaryOp {
                op: UnaryOp::Not,
                operand,
                span: Span::new(op_tok.span.start as usize, end as usize),
            }));
        }
        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Result<ExprId, String> {
        let mut expr = self.parse_primary()?;
        loop {
            if self.check(&TokenKind::Dot) {
                self.advance();
                let tok = self.peek().clone();
                match &tok.kind {
                    TokenKind::IntegerLiteral {
                        value,
                        suffix: None,
                    } => {
                        let index = *value as u32;
                        self.advance();
                        let start = self.expr_arena.get(expr).span().start;
                        expr = self.alloc_expr(Expr::TupleIndex {
                            expr,
                            index,
                            span: Span::new(start as usize, tok.span.end as usize),
                        });
                    }
                    TokenKind::Identifier(_) => {
                        let (field, field_span) = self.expect_identifier()?;
                        let start = self.expr_arena.get(expr).span().start;
                        expr = self.alloc_expr(Expr::FieldAccess {
                            expr,
                            field,
                            span: Span::new(start as usize, field_span.end as usize),
                        });
                    }
                    _ => {
                        return Err(self
                            .err_at_current("フィールド名または整数インデックスが期待されました"));
                    }
                }
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<ExprId, String> {
        let tok = self.peek().clone();
        match &tok.kind {
            TokenKind::IntegerLiteral { value, suffix } => {
                let value = *value;
                let suffix = *suffix;
                self.advance();
                Ok(self.alloc_expr(Expr::IntegerLiteral {
                    value,
                    suffix,
                    span: tok.span,
                }))
            }
            TokenKind::FloatLiteral { value, suffix } => {
                let value = *value;
                let suffix = *suffix;
                self.advance();
                Ok(self.alloc_expr(Expr::FloatLiteral {
                    value,
                    suffix,
                    span: tok.span,
                }))
            }
            TokenKind::True => {
                self.advance();
                Ok(self.alloc_expr(Expr::BoolLiteral {
                    value: true,
                    span: tok.span,
                }))
            }
            TokenKind::False => {
                self.advance();
                Ok(self.alloc_expr(Expr::BoolLiteral {
                    value: false,
                    span: tok.span,
                }))
            }
            TokenKind::Identifier(_) => {
                let (name, name_span) = self.expect_identifier()?;

                // Enum construction: Name::Variant(...)
                if self.check(&TokenKind::ColonColon) {
                    return self.parse_enum_expr(name, name_span);
                }

                // Struct expression: Name { field: expr, ... }
                if self.check(&TokenKind::LBrace) && self.is_struct_expr_start() {
                    return self.parse_struct_expr(name, name_span);
                }

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
                    let rparen = self.expect(&TokenKind::RParen)?;
                    Ok(self.alloc_expr(Expr::Call {
                        name,
                        args,
                        span: Span::new(name_span.start as usize, rparen.span.end as usize),
                    }))
                } else {
                    Ok(self.alloc_expr(Expr::Identifier {
                        name,
                        span: name_span,
                    }))
                }
            }
            TokenKind::LParen => {
                let start = tok.span.start;
                self.advance();
                // Unit literal ()
                if self.check(&TokenKind::RParen) {
                    let end_tok = self.advance().clone();
                    return Ok(self.alloc_expr(Expr::UnitLiteral {
                        span: Span::new(start as usize, end_tok.span.end as usize),
                    }));
                }
                let first = self.parse_expression()?;
                if self.check(&TokenKind::Comma) {
                    // Tuple expression
                    let mut elems = vec![first];
                    while self.check(&TokenKind::Comma) {
                        self.advance();
                        if !self.check(&TokenKind::RParen) {
                            elems.push(self.parse_expression()?);
                        }
                    }
                    let rparen = self.expect(&TokenKind::RParen)?;
                    Ok(self.alloc_expr(Expr::TupleExpr {
                        elements: elems,
                        span: Span::new(start as usize, rparen.span.end as usize),
                    }))
                } else {
                    self.expect(&TokenKind::RParen)?;
                    // Parenthesized expression - just return the inner expr
                    Ok(first)
                }
            }
            TokenKind::If => self.parse_if_expression(),
            TokenKind::While => self.parse_while_expression(None),
            TokenKind::Label(_) => {
                let label_tok = self.advance().clone();
                let label = if let TokenKind::Label(name) = label_tok.kind {
                    name
                } else {
                    unreachable!()
                };
                self.expect(&TokenKind::Colon)?;
                if self.check(&TokenKind::While) {
                    self.parse_while_expression(Some(label))
                } else {
                    Err(self.err_at_current("ラベルの後に while が期待されました"))
                }
            }
            TokenKind::Match => self.parse_match_expression(),
            TokenKind::LBrace => {
                let start = tok.span.start;
                let block = self.parse_block()?;
                let end = self.tokens[self.pos.saturating_sub(1)].span.end;
                Ok(self.alloc_expr(Expr::Block {
                    block,
                    span: Span::new(start as usize, end as usize),
                }))
            }
            _ => Err(self.err_expected("式")),
        }
    }

    fn is_struct_expr_start(&self) -> bool {
        if self.pos + 2 < self.tokens.len() {
            if matches!(&self.tokens[self.pos + 1].kind, TokenKind::Identifier(_))
                && matches!(&self.tokens[self.pos + 2].kind, TokenKind::Colon)
            {
                // Make sure it's not ::
                if self.pos + 3 < self.tokens.len()
                    && matches!(&self.tokens[self.pos + 3].kind, TokenKind::Colon)
                {
                    return false;
                }
                return true;
            }
            if let TokenKind::RBrace = &self.tokens[self.pos + 1].kind {
                return true;
            }
        }
        false
    }

    fn parse_struct_expr(&mut self, name: String, name_span: Span) -> Result<ExprId, String> {
        self.expect(&TokenKind::LBrace)?;
        let mut fields = Vec::new();
        while !self.check(&TokenKind::RBrace) && !self.check(&TokenKind::Eof) {
            let (field_name, _) = self.expect_identifier()?;
            self.expect(&TokenKind::Colon)?;
            let value = self.parse_expression()?;
            fields.push((field_name, value));
            if !self.check(&TokenKind::RBrace) && self.check(&TokenKind::Comma) {
                self.advance();
            }
        }
        let end_tok = self.expect(&TokenKind::RBrace)?;
        Ok(self.alloc_expr(Expr::StructExpr {
            name,
            fields,
            span: Span::new(name_span.start as usize, end_tok.span.end as usize),
        }))
    }

    fn parse_enum_expr(&mut self, enum_name: String, name_span: Span) -> Result<ExprId, String> {
        self.expect(&TokenKind::ColonColon)?;
        let (variant, _) = self.expect_identifier()?;
        let args = if self.check(&TokenKind::LParen) {
            self.advance();
            let mut exprs = Vec::new();
            if !self.check(&TokenKind::RParen) {
                exprs.push(self.parse_expression()?);
                while self.check(&TokenKind::Comma) {
                    self.advance();
                    exprs.push(self.parse_expression()?);
                }
            }
            self.expect(&TokenKind::RParen)?;
            EnumArgs::Tuple(exprs)
        } else if self.check(&TokenKind::LBrace) {
            self.advance();
            let mut fields = Vec::new();
            while !self.check(&TokenKind::RBrace) && !self.check(&TokenKind::Eof) {
                let (field_name, _) = self.expect_identifier()?;
                self.expect(&TokenKind::Colon)?;
                let value = self.parse_expression()?;
                fields.push((field_name, value));
                if !self.check(&TokenKind::RBrace) && self.check(&TokenKind::Comma) {
                    self.advance();
                }
            }
            self.expect(&TokenKind::RBrace)?;
            EnumArgs::Struct(fields)
        } else {
            EnumArgs::Unit
        };
        let end = self.tokens[self.pos.saturating_sub(1)].span.end;
        Ok(self.alloc_expr(Expr::EnumExpr {
            enum_name,
            variant,
            args,
            span: Span::new(name_span.start as usize, end as usize),
        }))
    }

    fn parse_if_expression(&mut self) -> Result<ExprId, String> {
        let if_tok = self.expect(&TokenKind::If)?;
        let start = if_tok.span.start;
        let condition = self.parse_expression()?;
        let then_block = self.parse_block()?;

        let else_block = if self.check(&TokenKind::Else) {
            self.advance();
            if self.check(&TokenKind::If) {
                let else_if = self.parse_if_expression()?;
                Some(ElseClause::ElseIf(else_if))
            } else {
                Some(ElseClause::ElseBlock(self.parse_block()?))
            }
        } else {
            None
        };

        let end = self.tokens[self.pos.saturating_sub(1)].span.end;
        Ok(self.alloc_expr(Expr::If {
            condition,
            then_block,
            else_block,
            span: Span::new(start as usize, end as usize),
        }))
    }

    fn parse_while_expression(&mut self, label: Option<String>) -> Result<ExprId, String> {
        let while_tok = self.expect(&TokenKind::While)?;
        let start = while_tok.span.start;
        let condition = self.parse_expression()?;
        let body = self.parse_block()?;
        let end = self.tokens[self.pos.saturating_sub(1)].span.end;
        Ok(self.alloc_expr(Expr::While {
            label,
            condition,
            body,
            span: Span::new(start as usize, end as usize),
        }))
    }

    fn parse_match_expression(&mut self) -> Result<ExprId, String> {
        let match_tok = self.expect(&TokenKind::Match)?;
        let start = match_tok.span.start;
        let expr = self.parse_expression()?;
        self.expect(&TokenKind::LBrace)?;
        let mut arms = Vec::new();
        while !self.check(&TokenKind::RBrace) && !self.check(&TokenKind::Eof) {
            let pattern = self.parse_pattern()?;
            self.expect(&TokenKind::FatArrow)?;
            let body = self.parse_expression()?;
            arms.push(MatchArm { pattern, body });
            if !self.check(&TokenKind::RBrace) && self.check(&TokenKind::Comma) {
                self.advance();
            }
        }
        let end_tok = self.expect(&TokenKind::RBrace)?;
        Ok(self.alloc_expr(Expr::Match {
            expr,
            arms,
            span: Span::new(start as usize, end_tok.span.end as usize),
        }))
    }

    fn parse_pattern(&mut self) -> Result<Pattern, String> {
        let tok = self.peek().clone();
        match &tok.kind {
            TokenKind::True => {
                self.advance();
                Ok(Pattern::BoolLiteral(true))
            }
            TokenKind::False => {
                self.advance();
                Ok(Pattern::BoolLiteral(false))
            }
            TokenKind::IntegerLiteral { value, .. } => {
                let value = *value;
                self.advance();
                if self.check(&TokenKind::DotDotEq) {
                    self.advance();
                    let end = self.parse_pattern_int()?;
                    Ok(Pattern::Range { start: value, end })
                } else {
                    Ok(Pattern::IntLiteral(value))
                }
            }
            TokenKind::Minus => {
                self.advance();
                if let TokenKind::IntegerLiteral { value, .. } = &self.peek().kind {
                    let value = -(*value);
                    self.advance();
                    if self.check(&TokenKind::DotDotEq) {
                        self.advance();
                        let end = self.parse_pattern_int()?;
                        Ok(Pattern::Range { start: value, end })
                    } else {
                        Ok(Pattern::IntLiteral(value))
                    }
                } else {
                    Err(self.err_at_current("パターン内の'-'の後に整数リテラルが期待されました"))
                }
            }
            TokenKind::Identifier(name) if name == "_" => {
                self.advance();
                Ok(Pattern::Wildcard)
            }
            TokenKind::Identifier(_) => {
                let (name, _) = self.expect_identifier()?;
                // Enum pattern: Name::Variant(...)
                if self.check(&TokenKind::ColonColon) {
                    self.advance();
                    let (variant, _) = self.expect_identifier()?;
                    let args = if self.check(&TokenKind::LParen) {
                        self.advance();
                        let mut patterns = Vec::new();
                        if !self.check(&TokenKind::RParen) {
                            patterns.push(self.parse_pattern()?);
                            while self.check(&TokenKind::Comma) {
                                self.advance();
                                if !self.check(&TokenKind::RParen) {
                                    patterns.push(self.parse_pattern()?);
                                }
                            }
                        }
                        self.expect(&TokenKind::RParen)?;
                        EnumPatternArgs::Tuple(patterns)
                    } else if self.check(&TokenKind::LBrace) {
                        self.advance();
                        let mut fields = Vec::new();
                        while !self.check(&TokenKind::RBrace) && !self.check(&TokenKind::Eof) {
                            let (field_name, _) = self.expect_identifier()?;
                            let pat = if self.check(&TokenKind::Colon) {
                                self.advance();
                                self.parse_pattern()?
                            } else {
                                Pattern::Binding(field_name.clone())
                            };
                            fields.push((field_name, pat));
                            if !self.check(&TokenKind::RBrace) && self.check(&TokenKind::Comma) {
                                self.advance();
                            }
                        }
                        self.expect(&TokenKind::RBrace)?;
                        EnumPatternArgs::Struct(fields)
                    } else {
                        EnumPatternArgs::Unit
                    };
                    Ok(Pattern::Enum {
                        enum_name: name,
                        variant,
                        args,
                    })
                } else if self.check(&TokenKind::LBrace) {
                    // Struct pattern: Name { field1, field2: pattern }
                    self.advance();
                    let mut fields = Vec::new();
                    while !self.check(&TokenKind::RBrace) && !self.check(&TokenKind::Eof) {
                        let (field_name, _) = self.expect_identifier()?;
                        let pat = if self.check(&TokenKind::Colon) {
                            self.advance();
                            self.parse_pattern()?
                        } else {
                            Pattern::Binding(field_name.clone())
                        };
                        fields.push((field_name, pat));
                        if !self.check(&TokenKind::RBrace) && self.check(&TokenKind::Comma) {
                            self.advance();
                        }
                    }
                    self.expect(&TokenKind::RBrace)?;
                    Ok(Pattern::Struct { name, fields })
                } else {
                    Ok(Pattern::Binding(name))
                }
            }
            TokenKind::LParen => {
                self.advance();
                let mut patterns = Vec::new();
                if !self.check(&TokenKind::RParen) {
                    patterns.push(self.parse_pattern()?);
                    while self.check(&TokenKind::Comma) {
                        self.advance();
                        if !self.check(&TokenKind::RParen) {
                            patterns.push(self.parse_pattern()?);
                        }
                    }
                }
                self.expect(&TokenKind::RParen)?;
                Ok(Pattern::Tuple(patterns))
            }
            _ => Err(self.err_expected("パターン")),
        }
    }

    fn parse_pattern_int(&mut self) -> Result<i64, String> {
        if self.check(&TokenKind::Minus) {
            self.advance();
            if let TokenKind::IntegerLiteral { value, .. } = &self.peek().kind {
                let value = -(*value);
                self.advance();
                Ok(value)
            } else {
                Err(self.err_at_current("範囲パターンの終端に整数リテラルが期待されました"))
            }
        } else if let TokenKind::IntegerLiteral { value, .. } = &self.peek().kind {
            let value = *value;
            self.advance();
            Ok(value)
        } else {
            Err(self.err_at_current("範囲パターンの終端に整数リテラルが期待されました"))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::lexer::Lexer;

    fn parse(source: &str) -> Result<ParseResult, String> {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().map_err(|e| e.to_string())?;
        let parser = Parser::new(tokens);
        parser.parse_program()
    }

    fn parse_ok(source: &str) -> ParseResult {
        parse(source).expect("parse should succeed")
    }

    #[test]
    fn empty_function() {
        let result = parse_ok("fn main() -> i32 { 0 }");
        assert_eq!(result.program.functions.len(), 1);
        assert_eq!(result.program.functions[0].name, "main");
    }

    #[test]
    fn function_with_params() {
        let result = parse_ok("fn add(a: i32, b: i32) -> i32 { a }");
        assert_eq!(result.program.functions[0].params.len(), 2);
    }

    #[test]
    fn let_statement() {
        let result = parse_ok("fn main() -> i32 { let x: i32 = 42; x }");
        assert_eq!(result.program.functions[0].body.stmts.len(), 1);
        assert!(result.program.functions[0].body.expr.is_some());
    }

    #[test]
    fn struct_def() {
        let result = parse_ok("struct Point { x: i32, y: i32 } fn main() -> i32 { 0 }");
        assert_eq!(result.program.structs.len(), 1);
        assert_eq!(result.program.structs[0].name, "Point");
        assert_eq!(result.program.structs[0].fields.len(), 2);
    }

    #[test]
    fn enum_def() {
        let result = parse_ok("enum Color { Red, Green, Blue } fn main() -> i32 { 0 }");
        assert_eq!(result.program.enums.len(), 1);
        assert_eq!(result.program.enums[0].variants.len(), 3);
    }
}
