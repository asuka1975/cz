use crate::diagnostics::Span;
use crate::syntax::token::{FloatSuffix, IntSuffix, Token, TokenKind};

pub struct Lexer<'a> {
    source: &'a [u8],
    pos: usize,
    /// line/column are kept for error messages (backward compatibility)
    line: usize,
    column: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source: source.as_bytes(),
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
                let span = Span::new(self.pos, self.pos);
                tokens.push(Token::new(TokenKind::Eof, span));
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

    fn peek(&self) -> u8 {
        if self.is_at_end() {
            0
        } else {
            self.source[self.pos]
        }
    }

    fn peek_next(&self) -> u8 {
        if self.pos + 1 >= self.source.len() {
            0
        } else {
            self.source[self.pos + 1]
        }
    }

    fn advance(&mut self) -> u8 {
        let ch = self.source[self.pos];
        self.pos += 1;
        if ch == b'\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        ch
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            while !self.is_at_end() && self.peek().is_ascii_whitespace() {
                self.advance();
            }
            if !self.is_at_end() && self.peek() == b'/' && self.peek_next() == b'/' {
                while !self.is_at_end() && self.peek() != b'\n' {
                    self.advance();
                }
                continue;
            }
            break;
        }
    }

    fn next_token(&mut self) -> Result<Token, String> {
        let start = self.pos;
        let line = self.line;
        let column = self.column;
        let ch = self.peek();

        // Number literal
        if ch.is_ascii_digit() {
            return self.read_number(start, line, column);
        }

        // Label: 'identifier
        if ch == b'\'' && self.peek_next().is_ascii_alphabetic() {
            self.advance(); // consume '
            let mut name = String::new();
            while !self.is_at_end() && (self.peek().is_ascii_alphanumeric() || self.peek() == b'_')
            {
                name.push(self.advance() as char);
            }
            return Ok(Token::new(
                TokenKind::Label(name),
                Span::new(start, self.pos),
            ));
        }

        // Identifier or keyword
        if ch.is_ascii_alphabetic() || ch == b'_' {
            return Ok(self.read_identifier_or_keyword(start));
        }

        // Operators and punctuation
        self.advance();
        let kind = match ch {
            b'+' => TokenKind::Plus,
            b'*' => TokenKind::Star,
            b'%' => TokenKind::Percent,
            b'(' => TokenKind::LParen,
            b')' => TokenKind::RParen,
            b'{' => TokenKind::LBrace,
            b'}' => TokenKind::RBrace,
            b';' => TokenKind::Semicolon,
            b',' => TokenKind::Comma,
            b':' => {
                if self.peek() == b':' {
                    self.advance();
                    TokenKind::ColonColon
                } else {
                    TokenKind::Colon
                }
            }
            b'.' => {
                if self.peek() == b'.' && self.peek_next() == b'=' {
                    self.advance();
                    self.advance();
                    TokenKind::DotDotEq
                } else {
                    TokenKind::Dot
                }
            }
            b'-' => {
                if self.peek() == b'>' {
                    self.advance();
                    TokenKind::Arrow
                } else {
                    TokenKind::Minus
                }
            }
            b'=' => {
                if self.peek() == b'=' {
                    self.advance();
                    TokenKind::EqEq
                } else if self.peek() == b'>' {
                    self.advance();
                    TokenKind::FatArrow
                } else {
                    TokenKind::Eq
                }
            }
            b'!' => {
                if self.peek() == b'=' {
                    self.advance();
                    TokenKind::NotEq
                } else {
                    TokenKind::Bang
                }
            }
            b'<' => {
                if self.peek() == b'=' {
                    self.advance();
                    TokenKind::LtEq
                } else {
                    TokenKind::Lt
                }
            }
            b'>' => {
                if self.peek() == b'=' {
                    self.advance();
                    TokenKind::GtEq
                } else {
                    TokenKind::Gt
                }
            }
            b'&' => {
                if self.peek() == b'&' {
                    self.advance();
                    TokenKind::AmpAmp
                } else {
                    return Err(format!("{}:{}: 予期しない文字 '&'", line, column));
                }
            }
            b'|' => {
                if self.peek() == b'|' {
                    self.advance();
                    TokenKind::PipePipe
                } else {
                    return Err(format!("{}:{}: 予期しない文字 '|'", line, column));
                }
            }
            b'/' => TokenKind::Slash,
            _ => {
                return Err(format!(
                    "{}:{}: 予期しない文字 '{}'",
                    line, column, ch as char
                ));
            }
        };

        Ok(Token::new(kind, Span::new(start, self.pos)))
    }

    fn read_number(&mut self, start: usize, line: usize, column: usize) -> Result<Token, String> {
        let mut num_str = String::new();
        while !self.is_at_end() && self.peek().is_ascii_digit() {
            num_str.push(self.advance() as char);
        }

        // Check for float (decimal point followed by digit)
        if self.peek() == b'.' && self.peek_next().is_ascii_digit() {
            num_str.push(self.advance() as char); // '.'
            while !self.is_at_end() && self.peek().is_ascii_digit() {
                num_str.push(self.advance() as char);
            }
            let suffix = self.read_float_suffix()?;
            let value: f64 = num_str
                .parse()
                .map_err(|_| format!("{}:{}: 浮動小数点リテラルが不正です", line, column))?;
            return Ok(Token::new(
                TokenKind::FloatLiteral { value, suffix },
                Span::new(start, self.pos),
            ));
        }

        // Check for type suffix on integer
        let suffix = self.read_int_or_float_suffix(line, column)?;
        match suffix {
            NumberSuffix::Int(s) => {
                let value: i64 = num_str
                    .parse()
                    .map_err(|_| format!("{}:{}: 整数リテラルが大きすぎます", line, column))?;
                Ok(Token::new(
                    TokenKind::IntegerLiteral { value, suffix: s },
                    Span::new(start, self.pos),
                ))
            }
            NumberSuffix::Float(s) => {
                let value: f64 = num_str
                    .parse()
                    .map_err(|_| format!("{}:{}: 浮動小数点リテラルが不正です", line, column))?;
                Ok(Token::new(
                    TokenKind::FloatLiteral {
                        value,
                        suffix: Some(s),
                    },
                    Span::new(start, self.pos),
                ))
            }
        }
    }

    fn read_float_suffix(&mut self) -> Result<Option<FloatSuffix>, String> {
        if self.peek() == b'f' {
            let line = self.line;
            let column = self.column;
            self.advance();
            let mut suffix_str = String::from("f");
            while !self.is_at_end() && self.peek().is_ascii_digit() {
                suffix_str.push(self.advance() as char);
            }
            match suffix_str.as_str() {
                "f32" => Ok(Some(FloatSuffix::F32)),
                "f64" => Ok(Some(FloatSuffix::F64)),
                _ => Err(format!(
                    "{}:{}: 不正な型サフィックス '{}'",
                    line, column, suffix_str
                )),
            }
        } else {
            Ok(None)
        }
    }

    fn read_int_or_float_suffix(
        &mut self,
        _line: usize,
        _column: usize,
    ) -> Result<NumberSuffix, String> {
        if self.peek() == b'i' {
            let suf_line = self.line;
            let suf_col = self.column;
            self.advance();
            let mut suffix_str = String::from("i");
            while !self.is_at_end() && self.peek().is_ascii_digit() {
                suffix_str.push(self.advance() as char);
            }
            match suffix_str.as_str() {
                "i8" => Ok(NumberSuffix::Int(Some(IntSuffix::I8))),
                "i16" => Ok(NumberSuffix::Int(Some(IntSuffix::I16))),
                "i32" => Ok(NumberSuffix::Int(Some(IntSuffix::I32))),
                "i64" => Ok(NumberSuffix::Int(Some(IntSuffix::I64))),
                _ => Err(format!(
                    "{}:{}: 不正な型サフィックス '{}'",
                    suf_line, suf_col, suffix_str
                )),
            }
        } else if self.peek() == b'f' {
            let suf_line = self.line;
            let suf_col = self.column;
            self.advance();
            let mut suffix_str = String::from("f");
            while !self.is_at_end() && self.peek().is_ascii_digit() {
                suffix_str.push(self.advance() as char);
            }
            match suffix_str.as_str() {
                "f32" => Ok(NumberSuffix::Float(FloatSuffix::F32)),
                "f64" => Ok(NumberSuffix::Float(FloatSuffix::F64)),
                _ => Err(format!(
                    "{}:{}: 不正な型サフィックス '{}'",
                    suf_line, suf_col, suffix_str
                )),
            }
        } else {
            Ok(NumberSuffix::Int(None))
        }
    }

    fn read_identifier_or_keyword(&mut self, start: usize) -> Token {
        let mut ident = String::new();
        while !self.is_at_end() && (self.peek().is_ascii_alphanumeric() || self.peek() == b'_') {
            ident.push(self.advance() as char);
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
            "match" => TokenKind::Match,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "struct" => TokenKind::Struct,
            "enum" => TokenKind::Enum,
            "as" => TokenKind::As,
            "i8" => TokenKind::I8,
            "i16" => TokenKind::I16,
            "i32" => TokenKind::I32,
            "i64" => TokenKind::I64,
            "f32" => TokenKind::F32,
            "f64" => TokenKind::F64,
            "bool" => TokenKind::Bool,
            _ => TokenKind::Identifier(ident),
        };
        Token::new(kind, Span::new(start, self.pos))
    }
}

enum NumberSuffix {
    Int(Option<IntSuffix>),
    Float(FloatSuffix),
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tokenize(source: &str) -> Vec<TokenKind> {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        tokens.into_iter().map(|t| t.kind).collect()
    }

    #[test]
    fn integer_literal() {
        assert_eq!(
            tokenize("42"),
            vec![
                TokenKind::IntegerLiteral {
                    value: 42,
                    suffix: None
                },
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn keywords() {
        assert_eq!(
            tokenize("fn let mut return if else while true false i32"),
            vec![
                TokenKind::Fn,
                TokenKind::Let,
                TokenKind::Mut,
                TokenKind::Return,
                TokenKind::If,
                TokenKind::Else,
                TokenKind::While,
                TokenKind::True,
                TokenKind::False,
                TokenKind::I32,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn operators() {
        assert_eq!(
            tokenize("+ - * / % == != < > <= >= && || !"),
            vec![
                TokenKind::Plus,
                TokenKind::Minus,
                TokenKind::Star,
                TokenKind::Slash,
                TokenKind::Percent,
                TokenKind::EqEq,
                TokenKind::NotEq,
                TokenKind::Lt,
                TokenKind::Gt,
                TokenKind::LtEq,
                TokenKind::GtEq,
                TokenKind::AmpAmp,
                TokenKind::PipePipe,
                TokenKind::Bang,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn span_tracking() {
        let mut lexer = Lexer::new("fn main");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0].span, Span::new(0, 2)); // "fn"
        assert_eq!(tokens[1].span, Span::new(3, 7)); // "main"
    }

    #[test]
    fn float_literal_with_suffix() {
        assert_eq!(
            tokenize("3.14f32"),
            vec![
                TokenKind::FloatLiteral {
                    value: 3.14,
                    suffix: Some(FloatSuffix::F32)
                },
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn integer_with_suffix() {
        assert_eq!(
            tokenize("42i64"),
            vec![
                TokenKind::IntegerLiteral {
                    value: 42,
                    suffix: Some(IntSuffix::I64)
                },
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn label_token() {
        assert_eq!(
            tokenize("'outer"),
            vec![TokenKind::Label("outer".into()), TokenKind::Eof]
        );
    }

    #[test]
    fn line_comment() {
        assert_eq!(
            tokenize("42 // comment\n7"),
            vec![
                TokenKind::IntegerLiteral {
                    value: 42,
                    suffix: None
                },
                TokenKind::IntegerLiteral {
                    value: 7,
                    suffix: None
                },
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn unexpected_character() {
        let mut lexer = Lexer::new("@");
        assert!(lexer.tokenize().is_err());
    }
}
