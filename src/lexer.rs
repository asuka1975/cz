use crate::token::{Token, TokenKind};

pub struct Lexer {
    source: Vec<char>,
    pos: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect(),
            pos: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, String> {
        let mut tokens = Vec::new();
        loop {
            self.skip_whitespace_and_comments();
            if self.is_at_end() {
                tokens.push(Token::new(TokenKind::Eof, self.line, self.column));
                break;
            }
            let token = self.next_token()?;
            tokens.push(token);
        }
        Ok(tokens)
    }

    fn is_at_end(&self) -> bool {
        self.pos >= self.source.len()
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source[self.pos]
        }
    }

    fn peek_next(&self) -> char {
        if self.pos + 1 >= self.source.len() {
            '\0'
        } else {
            self.source[self.pos + 1]
        }
    }

    fn advance(&mut self) -> char {
        let ch = self.source[self.pos];
        self.pos += 1;
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        ch
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            // Skip whitespace
            while !self.is_at_end() && self.peek().is_ascii_whitespace() {
                self.advance();
            }
            // Skip line comments
            if !self.is_at_end() && self.peek() == '/' && self.peek_next() == '/' {
                while !self.is_at_end() && self.peek() != '\n' {
                    self.advance();
                }
                continue;
            }
            break;
        }
    }

    fn next_token(&mut self) -> Result<Token, String> {
        let line = self.line;
        let column = self.column;
        let ch = self.peek();

        // Integer literal
        if ch.is_ascii_digit() {
            return self.read_integer(line, column);
        }

        // Identifier or keyword
        if ch.is_ascii_alphabetic() || ch == '_' {
            return Ok(self.read_identifier_or_keyword(line, column));
        }

        // Operators and punctuation
        self.advance();
        let kind = match ch {
            '+' => TokenKind::Plus,
            '*' => TokenKind::Star,
            '%' => TokenKind::Percent,
            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            '{' => TokenKind::LBrace,
            '}' => TokenKind::RBrace,
            ':' => TokenKind::Colon,
            ';' => TokenKind::Semicolon,
            ',' => TokenKind::Comma,
            '-' => {
                if self.peek() == '>' {
                    self.advance();
                    TokenKind::Arrow
                } else {
                    TokenKind::Minus
                }
            }
            '=' => {
                if self.peek() == '=' {
                    self.advance();
                    TokenKind::EqEq
                } else {
                    TokenKind::Eq
                }
            }
            '!' => {
                if self.peek() == '=' {
                    self.advance();
                    TokenKind::NotEq
                } else {
                    TokenKind::Bang
                }
            }
            '<' => {
                if self.peek() == '=' {
                    self.advance();
                    TokenKind::LtEq
                } else {
                    TokenKind::Lt
                }
            }
            '>' => {
                if self.peek() == '=' {
                    self.advance();
                    TokenKind::GtEq
                } else {
                    TokenKind::Gt
                }
            }
            '&' => {
                if self.peek() == '&' {
                    self.advance();
                    TokenKind::AmpAmp
                } else {
                    return Err(format!("{}:{}: 予期しない文字 '&'", line, column));
                }
            }
            '|' => {
                if self.peek() == '|' {
                    self.advance();
                    TokenKind::PipePipe
                } else {
                    return Err(format!("{}:{}: 予期しない文字 '|'", line, column));
                }
            }
            '/' => TokenKind::Slash,
            _ => {
                return Err(format!("{}:{}: 予期しない文字 '{}'", line, column, ch));
            }
        };

        Ok(Token::new(kind, line, column))
    }

    fn read_integer(&mut self, line: usize, column: usize) -> Result<Token, String> {
        let mut num_str = String::new();
        while !self.is_at_end() && self.peek().is_ascii_digit() {
            num_str.push(self.advance());
        }
        let value: i64 = num_str
            .parse()
            .map_err(|_| format!("{}:{}: 整数リテラルが大きすぎます", line, column))?;
        Ok(Token::new(TokenKind::IntegerLiteral(value), line, column))
    }

    fn read_identifier_or_keyword(&mut self, line: usize, column: usize) -> Token {
        let mut ident = String::new();
        while !self.is_at_end() && (self.peek().is_ascii_alphanumeric() || self.peek() == '_') {
            ident.push(self.advance());
        }
        let kind = match ident.as_str() {
            "fn" => TokenKind::Fn,
            "let" => TokenKind::Let,
            "mut" => TokenKind::Mut,
            "return" => TokenKind::Return,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "i32" => TokenKind::I32,
            _ => TokenKind::Identifier(ident),
        };
        Token::new(kind, line, column)
    }
}
