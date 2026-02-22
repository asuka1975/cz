#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Keywords
    Fn,
    Let,
    Mut,
    Return,
    If,
    Else,
    While,
    True,
    False,

    // Literals
    IntegerLiteral(i64),

    // Identifier
    Identifier(String),

    // Type keyword
    I32,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,

    // Comparison
    EqEq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,

    // Logical
    AmpAmp,
    PipePipe,
    Bang,

    // Assignment
    Eq,

    // Delimiters
    LParen,
    RParen,
    LBrace,
    RBrace,

    // Punctuation
    Colon,
    Semicolon,
    Comma,
    Arrow,

    // Special
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub column: usize,
}

impl Token {
    pub fn new(kind: TokenKind, line: usize, column: usize) -> Self {
        Self { kind, line, column }
    }
}
