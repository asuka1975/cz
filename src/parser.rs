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
