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
            vec![TokenKind::IntegerLiteral(42), TokenKind::Eof]
        );
    }

    #[test]
    fn multiple_integers() {
        assert_eq!(
            tokenize("1 23 456"),
            vec![
                TokenKind::IntegerLiteral(1),
                TokenKind::IntegerLiteral(23),
                TokenKind::IntegerLiteral(456),
                TokenKind::Eof,
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
    fn identifiers() {
        assert_eq!(
            tokenize("foo bar _x abc123"),
            vec![
                TokenKind::Identifier("foo".into()),
                TokenKind::Identifier("bar".into()),
                TokenKind::Identifier("_x".into()),
                TokenKind::Identifier("abc123".into()),
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn arithmetic_operators() {
        assert_eq!(
            tokenize("+ - * / %"),
            vec![
                TokenKind::Plus,
                TokenKind::Minus,
                TokenKind::Star,
                TokenKind::Slash,
                TokenKind::Percent,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn comparison_operators() {
        assert_eq!(
            tokenize("== != < > <= >="),
            vec![
                TokenKind::EqEq,
                TokenKind::NotEq,
                TokenKind::Lt,
                TokenKind::Gt,
                TokenKind::LtEq,
                TokenKind::GtEq,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn logical_operators() {
        assert_eq!(
            tokenize("&& || !"),
            vec![
                TokenKind::AmpAmp,
                TokenKind::PipePipe,
                TokenKind::Bang,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn delimiters_and_punctuation() {
        assert_eq!(
            tokenize("( ) { } : ; , ->"),
            vec![
                TokenKind::LParen,
                TokenKind::RParen,
                TokenKind::LBrace,
                TokenKind::RBrace,
                TokenKind::Colon,
                TokenKind::Semicolon,
                TokenKind::Comma,
                TokenKind::Arrow,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn assignment_vs_equality() {
        assert_eq!(
            tokenize("= =="),
            vec![TokenKind::Eq, TokenKind::EqEq, TokenKind::Eof]
        );
    }

    #[test]
    fn arrow_vs_minus() {
        assert_eq!(
            tokenize("- ->"),
            vec![TokenKind::Minus, TokenKind::Arrow, TokenKind::Eof]
        );
    }

    #[test]
    fn line_comment() {
        assert_eq!(
            tokenize("42 // this is a comment\n7"),
            vec![
                TokenKind::IntegerLiteral(42),
                TokenKind::IntegerLiteral(7),
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn comment_only() {
        assert_eq!(tokenize("// nothing here"), vec![TokenKind::Eof]);
    }

    #[test]
    fn line_column_tracking() {
        let mut lexer = Lexer::new("fn main\n  42");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0].line, 1);
        assert_eq!(tokens[0].column, 1); // fn
        assert_eq!(tokens[1].line, 1);
        assert_eq!(tokens[1].column, 4); // main
        assert_eq!(tokens[2].line, 2);
        assert_eq!(tokens[2].column, 3); // 42
    }

    #[test]
    fn unexpected_character() {
        let mut lexer = Lexer::new("@");
        let result = lexer.tokenize();
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("予期しない文字"));
    }

    #[test]
    fn single_ampersand_error() {
        let mut lexer = Lexer::new("&");
        let result = lexer.tokenize();
        assert!(result.is_err());
    }

    #[test]
    fn single_pipe_error() {
        let mut lexer = Lexer::new("|");
        let result = lexer.tokenize();
        assert!(result.is_err());
    }

    #[test]
    fn empty_source() {
        assert_eq!(tokenize(""), vec![TokenKind::Eof]);
    }

    #[test]
    fn whitespace_only() {
        assert_eq!(tokenize("   \n\t  \n  "), vec![TokenKind::Eof]);
    }

    #[test]
    fn function_signature() {
        assert_eq!(
            tokenize("fn add(a: i32, b: i32) -> i32"),
            vec![
                TokenKind::Fn,
                TokenKind::Identifier("add".into()),
                TokenKind::LParen,
                TokenKind::Identifier("a".into()),
                TokenKind::Colon,
                TokenKind::I32,
                TokenKind::Comma,
                TokenKind::Identifier("b".into()),
                TokenKind::Colon,
                TokenKind::I32,
                TokenKind::RParen,
                TokenKind::Arrow,
                TokenKind::I32,
                TokenKind::Eof,
            ]
        );
    }
}
