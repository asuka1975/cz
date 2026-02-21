/// A token produced by the lexer.
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Literals
    Integer(i64),
    Identifier(String),

    // Keywords
    Fn,
    Return,
    Int,
    Void,

    // Symbols
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Semicolon,
    Colon,
    Arrow,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,

    Eof,
}

/// Tokenizes the given source string into a list of tokens.
pub fn tokenize(source: &str) -> Result<Vec<Token>, LexError> {
    let mut tokens = Vec::new();
    let mut chars = source.chars().peekable();

    while let Some(&ch) = chars.peek() {
        match ch {
            // Skip whitespace
            c if c.is_ascii_whitespace() => {
                chars.next();
            }

            // Skip line comments
            '/' if matches!(chars.clone().nth(1), Some('/')) => {
                for c in chars.by_ref() {
                    if c == '\n' {
                        break;
                    }
                }
            }

            // Integer literals
            c if c.is_ascii_digit() => {
                let mut num = String::new();
                while let Some(&d) = chars.peek() {
                    if d.is_ascii_digit() {
                        num.push(d);
                        chars.next();
                    } else {
                        break;
                    }
                }
                let value = num
                    .parse::<i64>()
                    .map_err(|_| LexError::InvalidInteger(num))?;
                tokens.push(Token::Integer(value));
            }

            // Identifiers and keywords
            c if c.is_alphabetic() || c == '_' => {
                let mut ident = String::new();
                while let Some(&d) = chars.peek() {
                    if d.is_alphanumeric() || d == '_' {
                        ident.push(d);
                        chars.next();
                    } else {
                        break;
                    }
                }
                let tok = match ident.as_str() {
                    "fn" => Token::Fn,
                    "return" => Token::Return,
                    "int" => Token::Int,
                    "void" => Token::Void,
                    _ => Token::Identifier(ident),
                };
                tokens.push(tok);
            }

            // Symbols and operators
            '(' => {
                tokens.push(Token::LParen);
                chars.next();
            }
            ')' => {
                tokens.push(Token::RParen);
                chars.next();
            }
            '{' => {
                tokens.push(Token::LBrace);
                chars.next();
            }
            '}' => {
                tokens.push(Token::RBrace);
                chars.next();
            }
            ',' => {
                tokens.push(Token::Comma);
                chars.next();
            }
            ';' => {
                tokens.push(Token::Semicolon);
                chars.next();
            }
            ':' => {
                tokens.push(Token::Colon);
                chars.next();
            }
            '+' => {
                tokens.push(Token::Plus);
                chars.next();
            }
            '*' => {
                tokens.push(Token::Star);
                chars.next();
            }
            '/' => {
                tokens.push(Token::Slash);
                chars.next();
            }
            '-' => {
                chars.next();
                if chars.peek() == Some(&'>') {
                    chars.next();
                    tokens.push(Token::Arrow);
                } else {
                    tokens.push(Token::Minus);
                }
            }

            c => {
                return Err(LexError::UnexpectedChar(c));
            }
        }
    }

    tokens.push(Token::Eof);
    Ok(tokens)
}

/// Errors that can occur during lexing.
#[derive(Debug, Clone, PartialEq)]
pub enum LexError {
    UnexpectedChar(char),
    InvalidInteger(String),
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexError::UnexpectedChar(c) => write!(f, "unexpected character: '{c}'"),
            LexError::InvalidInteger(s) => write!(f, "invalid integer literal: '{s}'"),
        }
    }
}

impl std::error::Error for LexError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_empty() {
        let tokens = tokenize("").unwrap();
        assert_eq!(tokens, vec![Token::Eof]);
    }

    #[test]
    fn test_tokenize_integer() {
        let tokens = tokenize("42").unwrap();
        assert_eq!(tokens, vec![Token::Integer(42), Token::Eof]);
    }

    #[test]
    fn test_tokenize_keywords() {
        let tokens = tokenize("fn return int void").unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Fn,
                Token::Return,
                Token::Int,
                Token::Void,
                Token::Eof
            ]
        );
    }

    #[test]
    fn test_tokenize_identifier() {
        let tokens = tokenize("foo_bar").unwrap();
        assert_eq!(
            tokens,
            vec![Token::Identifier("foo_bar".to_string()), Token::Eof]
        );
    }

    #[test]
    fn test_tokenize_arrow() {
        let tokens = tokenize("->").unwrap();
        assert_eq!(tokens, vec![Token::Arrow, Token::Eof]);
    }

    #[test]
    fn test_unexpected_char() {
        let err = tokenize("@").unwrap_err();
        assert_eq!(err, LexError::UnexpectedChar('@'));
    }
}
