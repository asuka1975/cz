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
                    tok.line, tok.column, tok.kind
                ));
            }
        }
        Ok(Program {
            functions,
            structs,
            enums,
        })
    }

    // --- Helpers ---

    fn expr_ends_with_block(expr: &Expr) -> bool {
        matches!(
            expr,
            Expr::If { .. }
                | Expr::Block(_)
                | Expr::Assign { .. }
                | Expr::While { .. }
                | Expr::Match { .. }
        )
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

    // --- Struct/Enum Definitions ---

    fn parse_struct_def(&mut self) -> Result<StructDef, String> {
        let struct_tok = self.expect(&TokenKind::Struct)?;
        let line = struct_tok.line;
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
            if !self.check(&TokenKind::RBrace) {
                // Allow optional trailing comma
                if self.check(&TokenKind::Comma) {
                    self.advance();
                }
            }
        }
        self.expect(&TokenKind::RBrace)?;
        Ok(StructDef { name, fields, line })
    }

    fn parse_enum_def(&mut self) -> Result<EnumDef, String> {
        let enum_tok = self.expect(&TokenKind::Enum)?;
        let line = enum_tok.line;
        let (name, _) = self.expect_identifier()?;
        self.expect(&TokenKind::LBrace)?;
        let mut variants = Vec::new();
        while !self.check(&TokenKind::RBrace) && !self.check(&TokenKind::Eof) {
            let (variant_name, _) = self.expect_identifier()?;
            let kind = if self.check(&TokenKind::LParen) {
                // Tuple variant
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
                // Struct variant
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
        self.expect(&TokenKind::RBrace)?;
        Ok(EnumDef {
            name,
            variants,
            line,
        })
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

        let return_type = if self.check(&TokenKind::Arrow) {
            self.advance();
            self.parse_type()?
        } else {
            Type::Unit
        };

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
                // Tuple type
                let mut types = vec![self.parse_type()?];
                while self.check(&TokenKind::Comma) {
                    self.advance();
                    if !self.check(&TokenKind::RParen) {
                        types.push(self.parse_type()?);
                    }
                }
                self.expect(&TokenKind::RParen)?;
                if types.len() == 1 {
                    // Just parenthesized type
                    Ok(types.into_iter().next().unwrap())
                } else {
                    Ok(Type::Tuple(types))
                }
            }
            TokenKind::Identifier(_) => {
                let (name, _) = self.expect_identifier()?;
                Ok(Type::Named(name))
            }
            _ => Err(format!(
                "{}:{}: 型が期待されましたが {:?} が見つかりました",
                tok.line, tok.column, tok.kind
            )),
        }
    }

    // --- Block ---

    fn parse_block(&mut self) -> Result<Block, String> {
        self.expect(&TokenKind::LBrace)?;

        let mut stmts = Vec::new();
        let mut tail_expr: Option<Box<Expr>> = None;

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
                // Parse as expression, then decide if it's a statement or tail expression
                let expr = self.parse_expression()?;

                if self.check(&TokenKind::Semicolon) {
                    self.advance();
                    stmts.push(Stmt::Expr(expr));
                } else if self.check(&TokenKind::RBrace) {
                    // Tail expression (last expression before closing brace)
                    tail_expr = Some(Box::new(expr));
                } else if Self::expr_ends_with_block(&expr) {
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

        let var_type = if self.check(&TokenKind::Colon) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

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

    fn parse_return_stmt(&mut self) -> Result<Stmt, String> {
        let ret_tok = self.expect(&TokenKind::Return)?;
        let line = ret_tok.line;
        if self.check(&TokenKind::Semicolon) {
            self.advance();
            Ok(Stmt::Return { value: None, line })
        } else {
            let value = self.parse_expression()?;
            self.expect(&TokenKind::Semicolon)?;
            Ok(Stmt::Return {
                value: Some(value),
                line,
            })
        }
    }

    fn parse_break_stmt(&mut self) -> Result<Stmt, String> {
        let break_tok = self.expect(&TokenKind::Break)?;
        let line = break_tok.line;
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
        self.expect(&TokenKind::Semicolon)?;
        Ok(Stmt::Break { label, value, line })
    }

    fn parse_continue_stmt(&mut self) -> Result<Stmt, String> {
        let cont_tok = self.expect(&TokenKind::Continue)?;
        let line = cont_tok.line;
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
        self.expect(&TokenKind::Semicolon)?;
        Ok(Stmt::Continue { label, line })
    }

    // --- Expressions (precedence climbing) ---

    fn parse_expression(&mut self) -> Result<Expr, String> {
        // Check for assignment: identifier = expr
        if let TokenKind::Identifier(_) = &self.peek().kind
            && self.pos + 1 < self.tokens.len()
            && let TokenKind::Eq = &self.tokens[self.pos + 1].kind
        {
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
            left = Expr::BinaryOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_cast(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_unary()?;
        while self.check(&TokenKind::As) {
            self.advance();
            let target_type = self.parse_type()?;
            expr = Expr::Cast {
                expr: Box::new(expr),
                target_type,
            };
        }
        Ok(expr)
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
        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_primary()?;
        loop {
            if self.check(&TokenKind::Dot) {
                self.advance();
                // Check for tuple index (integer literal) or field name
                let tok = self.peek().clone();
                match &tok.kind {
                    TokenKind::IntegerLiteral {
                        value,
                        suffix: None,
                    } => {
                        let index = *value as u32;
                        self.advance();
                        expr = Expr::TupleIndex {
                            expr: Box::new(expr),
                            index,
                        };
                    }
                    TokenKind::Identifier(_) => {
                        let (field, _) = self.expect_identifier()?;
                        expr = Expr::FieldAccess {
                            expr: Box::new(expr),
                            field,
                        };
                    }
                    _ => {
                        return Err(format!(
                            "{}:{}: フィールド名または整数インデックスが期待されました",
                            tok.line, tok.column
                        ));
                    }
                }
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
        let tok = self.peek().clone();
        match &tok.kind {
            TokenKind::IntegerLiteral { value, suffix } => {
                let value = *value;
                let suffix = *suffix;
                self.advance();
                Ok(Expr::IntegerLiteral { value, suffix })
            }
            TokenKind::FloatLiteral { value, suffix } => {
                let value = *value;
                let suffix = *suffix;
                self.advance();
                Ok(Expr::FloatLiteral { value, suffix })
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

                // Check for :: (enum construction)
                if self.check(&TokenKind::ColonColon) {
                    return self.parse_enum_expr(name, line);
                }

                // Check for struct expression: Name { field: expr, ... }
                if self.check(&TokenKind::LBrace) {
                    // Look ahead to determine if this is a struct expression or a block
                    // struct expression: Name { ident : ...
                    if self.is_struct_expr_start() {
                        return self.parse_struct_expr(name, line);
                    }
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
                    self.expect(&TokenKind::RParen)?;
                    Ok(Expr::Call { name, args, line })
                } else {
                    Ok(Expr::Identifier(name, line))
                }
            }
            TokenKind::LParen => {
                self.advance();
                // Check for unit literal ()
                if self.check(&TokenKind::RParen) {
                    self.advance();
                    return Ok(Expr::UnitLiteral);
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
                    self.expect(&TokenKind::RParen)?;
                    Ok(Expr::TupleExpr(elems))
                } else {
                    self.expect(&TokenKind::RParen)?;
                    Ok(first)
                }
            }
            TokenKind::If => self.parse_if_expression(),
            TokenKind::While => self.parse_while_expression(None),
            TokenKind::Label(_) => {
                // Labeled while: 'label: while ...
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
                    let tok = self.peek();
                    Err(format!(
                        "{}:{}: ラベルの後に while が期待されました",
                        tok.line, tok.column
                    ))
                }
            }
            TokenKind::Match => self.parse_match_expression(),
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

    fn is_struct_expr_start(&self) -> bool {
        // Look after '{' for 'ident :'
        if self.pos + 2 < self.tokens.len() {
            if let TokenKind::Identifier(_) = &self.tokens[self.pos + 1].kind
                && let TokenKind::Colon = &self.tokens[self.pos + 2].kind
            {
                // Make sure it's not ::
                if self.pos + 3 < self.tokens.len()
                    && let TokenKind::Colon = &self.tokens[self.pos + 3].kind
                {
                    return false;
                }
                return true;
            }
            // Also check for empty struct: Name { }
            if let TokenKind::RBrace = &self.tokens[self.pos + 1].kind {
                return true;
            }
        }
        false
    }

    fn parse_struct_expr(&mut self, name: String, line: usize) -> Result<Expr, String> {
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
        self.expect(&TokenKind::RBrace)?;
        Ok(Expr::StructExpr { name, fields, line })
    }

    fn parse_enum_expr(&mut self, enum_name: String, line: usize) -> Result<Expr, String> {
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
        Ok(Expr::EnumExpr {
            enum_name,
            variant,
            args,
            line,
        })
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

    fn parse_while_expression(&mut self, label: Option<String>) -> Result<Expr, String> {
        self.expect(&TokenKind::While)?;
        let condition = self.parse_expression()?;
        let body = self.parse_block()?;
        Ok(Expr::While {
            label,
            condition: Box::new(condition),
            body,
        })
    }

    fn parse_match_expression(&mut self) -> Result<Expr, String> {
        self.expect(&TokenKind::Match)?;
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
        self.expect(&TokenKind::RBrace)?;
        Ok(Expr::Match {
            expr: Box::new(expr),
            arms,
        })
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
                // Check for range pattern
                if self.check(&TokenKind::DotDotEq) {
                    self.advance();
                    let end = self.parse_pattern_int()?;
                    Ok(Pattern::Range { start: value, end })
                } else {
                    Ok(Pattern::IntLiteral(value))
                }
            }
            TokenKind::Minus => {
                // Negative integer pattern
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
                    let tok = self.peek();
                    Err(format!(
                        "{}:{}: パターン内の'-'の後に整数リテラルが期待されました",
                        tok.line, tok.column
                    ))
                }
            }
            TokenKind::Identifier(name) => {
                let name = name.clone();
                let line = tok.line;
                self.advance();
                // Check for enum pattern: Name::Variant(...)
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
                    // Check if this is a known struct/enum name or just a binding
                    // At parse time we can't know, so we treat lowercase-start as binding
                    // and uppercase-start as a potential type name
                    // Actually, we'll just use Binding for all identifiers at parse time;
                    // semantic analysis will resolve
                    let _ = line;
                    Ok(Pattern::Binding(name))
                }
            }
            TokenKind::LParen => {
                // Tuple pattern
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
            _ => {
                // Check for wildcard '_'
                if let TokenKind::Identifier(name) = &tok.kind
                    && name == "_"
                {
                    self.advance();
                    return Ok(Pattern::Wildcard);
                }
                Err(format!(
                    "{}:{}: パターンが期待されましたが {:?} が見つかりました",
                    tok.line, tok.column, tok.kind
                ))
            }
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
                let tok = self.peek();
                Err(format!(
                    "{}:{}: 範囲パターンの終端に整数リテラルが期待されました",
                    tok.line, tok.column
                ))
            }
        } else if let TokenKind::IntegerLiteral { value, .. } = &self.peek().kind {
            let value = *value;
            self.advance();
            Ok(value)
        } else {
            let tok = self.peek();
            Err(format!(
                "{}:{}: 範囲パターンの終端に整数リテラルが期待されました",
                tok.line, tok.column
            ))
        }
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
        if let Stmt::Let { name, mutable, .. } = &prog.functions[0].body.stmts[0] {
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
    fn while_expression() {
        let prog = parse_ok("fn main() -> i32 { while true { 0; } 0 }");
        if let Stmt::Expr(Expr::While { .. }) = &prog.functions[0].body.stmts[0] {
            // ok
        } else {
            panic!("expected While expression");
        }
    }

    #[test]
    fn integer_literal_expr() {
        let prog = parse_ok("fn main() -> i32 { 42 }");
        if let Some(expr) = &prog.functions[0].body.expr {
            assert!(matches!(
                expr.as_ref(),
                Expr::IntegerLiteral {
                    value: 42,
                    suffix: None
                }
            ));
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
                    Expr::BinaryOp { op: BinOp::Mul, .. }
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
        let prog = parse_ok(
            "fn main() -> i32 { let a: i32 = 1; let b: i32 = 2; let c: i32 = 3; let d: i32 = 4; if a < b && c > d { 1 } else { 0 } }",
        );
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
        let prog = parse_ok("fn main() -> i32 { if true { 1 } else if false { 2 } else { 3 } }");
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
            if let Expr::BinaryOp {
                op: BinOp::Mul,
                left,
                ..
            } = expr.as_ref()
            {
                assert!(matches!(
                    left.as_ref(),
                    Expr::BinaryOp { op: BinOp::Add, .. }
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

    // MS2 tests

    #[test]
    fn let_type_inference() {
        let prog = parse_ok("fn main() -> i32 { let x = 42; x }");
        if let Stmt::Let { var_type, .. } = &prog.functions[0].body.stmts[0] {
            assert_eq!(*var_type, None);
        } else {
            panic!("expected Let statement");
        }
    }

    #[test]
    fn unit_return_type() {
        let prog = parse_ok("fn foo() { }");
        assert_eq!(prog.functions[0].return_type, Type::Unit);
    }

    #[test]
    fn cast_expression() {
        let prog = parse_ok("fn main() -> i32 { 42i64 as i32 }");
        if let Some(expr) = &prog.functions[0].body.expr {
            assert!(matches!(expr.as_ref(), Expr::Cast { .. }));
        } else {
            panic!("expected tail expression");
        }
    }

    #[test]
    fn struct_definition() {
        let prog = parse_ok(
            "struct Point { x: i32, y: i32 }
             fn main() -> i32 { 0 }",
        );
        assert_eq!(prog.structs.len(), 1);
        assert_eq!(prog.structs[0].name, "Point");
        assert_eq!(prog.structs[0].fields.len(), 2);
    }

    #[test]
    fn enum_definition() {
        let prog = parse_ok(
            "enum Color { Red, Green, Blue }
             fn main() -> i32 { 0 }",
        );
        assert_eq!(prog.enums.len(), 1);
        assert_eq!(prog.enums[0].name, "Color");
        assert_eq!(prog.enums[0].variants.len(), 3);
    }

    #[test]
    fn match_expression_parse() {
        let prog = parse_ok("fn main() -> i32 { match 1 { 0 => 10, 1 => 20, _ => 30 } }");
        if let Some(Expr::Match { arms, .. }) = prog.functions[0].body.expr.as_deref() {
            assert_eq!(arms.len(), 3);
        } else {
            panic!("expected Match expression");
        }
    }

    #[test]
    fn break_continue_parse() {
        let prog = parse_ok(
            "fn main() -> i32 {
                let mut x: i32 = 0;
                while x < 10 {
                    x = x + 1;
                    if x == 5 { break; }
                    if x == 3 { continue; }
                }
                x
            }",
        );
        assert_eq!(prog.functions.len(), 1);
    }

    #[test]
    fn tuple_expression_parse() {
        let prog = parse_ok("fn main() -> i32 { let t = (1, 2, 3); t.0 }");
        assert_eq!(prog.functions.len(), 1);
    }

    #[test]
    fn field_access_parse() {
        let prog = parse_ok(
            "struct Point { x: i32, y: i32 }
             fn main() -> i32 { let p = Point { x: 1, y: 2 }; p.x }",
        );
        assert_eq!(prog.functions.len(), 1);
    }
}
