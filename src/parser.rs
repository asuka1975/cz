use crate::ast::{BinaryOp, Block, Declaration, Expr, FunctionDecl, Param, Program, Stmt, Type};
use crate::lexer::Token;

/// Parses a flat token slice into a [`Program`] AST.
pub fn parse(tokens: &[Token]) -> Result<Program, ParseError> {
    let mut parser = Parser::new(tokens);
    parser.parse_program()
}

struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Self { tokens, pos: 0 }
    }

    fn peek(&self) -> &Token {
        self.tokens.get(self.pos).unwrap_or(&Token::Eof)
    }

    fn advance(&mut self) -> &Token {
        let tok = self.tokens.get(self.pos).unwrap_or(&Token::Eof);
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
        tok
    }

    fn expect(&mut self, expected: &Token) -> Result<(), ParseError> {
        let tok = self.advance();
        if tok == expected {
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: format!("{expected:?}"),
                found: format!("{tok:?}"),
            })
        }
    }

    fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut declarations = Vec::new();
        while self.peek() != &Token::Eof {
            declarations.push(self.parse_declaration()?);
        }
        Ok(Program { declarations })
    }

    fn parse_declaration(&mut self) -> Result<Declaration, ParseError> {
        match self.peek() {
            Token::Fn => Ok(Declaration::Function(self.parse_function()?)),
            tok => Err(ParseError::UnexpectedToken {
                expected: "declaration".to_string(),
                found: format!("{tok:?}"),
            }),
        }
    }

    fn parse_function(&mut self) -> Result<FunctionDecl, ParseError> {
        self.expect(&Token::Fn)?;

        let name = match self.advance() {
            Token::Identifier(n) => n.clone(),
            tok => {
                return Err(ParseError::UnexpectedToken {
                    expected: "function name".to_string(),
                    found: format!("{tok:?}"),
                })
            }
        };

        self.expect(&Token::LParen)?;
        let params = self.parse_params()?;
        self.expect(&Token::RParen)?;

        let return_type = if self.peek() == &Token::Arrow {
            self.advance();
            self.parse_type()?
        } else {
            Type::Void
        };

        let body = self.parse_block()?;

        Ok(FunctionDecl {
            name,
            params,
            return_type,
            body,
        })
    }

    fn parse_params(&mut self) -> Result<Vec<Param>, ParseError> {
        let mut params = Vec::new();
        if self.peek() == &Token::RParen {
            return Ok(params);
        }
        loop {
            let name = match self.advance() {
                Token::Identifier(n) => n.clone(),
                tok => {
                    return Err(ParseError::UnexpectedToken {
                        expected: "parameter name".to_string(),
                        found: format!("{tok:?}"),
                    })
                }
            };
            self.expect(&Token::Colon)?;
            let ty = self.parse_type()?;
            params.push(Param { name, ty });
            if self.peek() != &Token::Comma {
                break;
            }
            self.advance();
        }
        Ok(params)
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        match self.advance() {
            Token::Int => Ok(Type::Int),
            Token::Void => Ok(Type::Void),
            Token::Identifier(n) => Ok(Type::Named(n.clone())),
            tok => Err(ParseError::UnexpectedToken {
                expected: "type".to_string(),
                found: format!("{tok:?}"),
            }),
        }
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        self.expect(&Token::LBrace)?;
        let mut stmts = Vec::new();
        while self.peek() != &Token::RBrace && self.peek() != &Token::Eof {
            stmts.push(self.parse_stmt()?);
        }
        self.expect(&Token::RBrace)?;
        Ok(Block { stmts })
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        match self.peek() {
            Token::Return => {
                self.advance();
                if self.peek() == &Token::Semicolon {
                    self.advance();
                    Ok(Stmt::Return(None))
                } else {
                    let expr = self.parse_expr()?;
                    self.expect(&Token::Semicolon)?;
                    Ok(Stmt::Return(Some(expr)))
                }
            }
            _ => {
                let expr = self.parse_expr()?;
                self.expect(&Token::Semicolon)?;
                Ok(Stmt::Expr(expr))
            }
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_additive()
    }

    fn parse_additive(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_multiplicative()?;
        loop {
            let op = match self.peek() {
                Token::Plus => BinaryOp::Add,
                Token::Minus => BinaryOp::Sub,
                _ => break,
            };
            self.advance();
            let rhs = self.parse_multiplicative()?;
            lhs = Expr::BinaryOp {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }
        Ok(lhs)
    }

    fn parse_multiplicative(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_primary()?;
        loop {
            let op = match self.peek() {
                Token::Star => BinaryOp::Mul,
                Token::Slash => BinaryOp::Div,
                _ => break,
            };
            self.advance();
            let rhs = self.parse_primary()?;
            lhs = Expr::BinaryOp {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }
        Ok(lhs)
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        match self.peek().clone() {
            Token::Integer(n) => {
                self.advance();
                Ok(Expr::Integer(n))
            }
            Token::Identifier(name) => {
                let name = name.clone();
                self.advance();
                if self.peek() == &Token::LParen {
                    self.advance();
                    let args = self.parse_args()?;
                    self.expect(&Token::RParen)?;
                    Ok(Expr::Call { callee: name, args })
                } else {
                    Ok(Expr::Identifier(name))
                }
            }
            Token::LParen => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect(&Token::RParen)?;
                Ok(expr)
            }
            tok => Err(ParseError::UnexpectedToken {
                expected: "expression".to_string(),
                found: format!("{tok:?}"),
            }),
        }
    }

    fn parse_args(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut args = Vec::new();
        if self.peek() == &Token::RParen {
            return Ok(args);
        }
        loop {
            args.push(self.parse_expr()?);
            if self.peek() != &Token::Comma {
                break;
            }
            self.advance();
        }
        Ok(args)
    }
}

/// Errors that can occur during parsing.
#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    UnexpectedToken { expected: String, found: String },
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken { expected, found } => {
                write!(f, "expected {expected}, found {found}")
            }
        }
    }
}

impl std::error::Error for ParseError {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokenize;

    fn parse_src(src: &str) -> Result<Program, ParseError> {
        let tokens = tokenize(src).expect("lex error");
        parse(&tokens)
    }

    #[test]
    fn test_parse_empty_program() {
        let program = parse_src("").unwrap();
        assert!(program.declarations.is_empty());
    }

    #[test]
    fn test_parse_void_function() {
        let program = parse_src("fn main() {}").unwrap();
        assert_eq!(program.declarations.len(), 1);
        let Declaration::Function(f) = &program.declarations[0];
        assert_eq!(f.name, "main");
        assert!(f.params.is_empty());
        assert_eq!(f.return_type, Type::Void);
        assert!(f.body.stmts.is_empty());
    }

    #[test]
    fn test_parse_function_with_return() {
        let program = parse_src("fn answer() -> int { return 42; }").unwrap();
        let Declaration::Function(f) = &program.declarations[0];
        assert_eq!(f.return_type, Type::Int);
        assert_eq!(f.body.stmts.len(), 1);
        assert_eq!(f.body.stmts[0], Stmt::Return(Some(Expr::Integer(42))));
    }

    #[test]
    fn test_parse_binary_expr() {
        let program = parse_src("fn f() -> int { return 1 + 2 * 3; }").unwrap();
        let Declaration::Function(f) = &program.declarations[0];
        let Stmt::Return(Some(expr)) = &f.body.stmts[0] else {
            panic!("expected return");
        };
        // 1 + (2 * 3)
        assert_eq!(
            *expr,
            Expr::BinaryOp {
                op: BinaryOp::Add,
                lhs: Box::new(Expr::Integer(1)),
                rhs: Box::new(Expr::BinaryOp {
                    op: BinaryOp::Mul,
                    lhs: Box::new(Expr::Integer(2)),
                    rhs: Box::new(Expr::Integer(3)),
                }),
            }
        );
    }
}
