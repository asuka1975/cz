#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntSuffix {
    I8,
    I16,
    I32,
    I64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FloatSuffix {
    F32,
    F64,
}

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
    Match,
    Break,
    Continue,
    Struct,
    Enum,
    As,

    // Literals
    IntegerLiteral {
        value: i64,
        suffix: Option<IntSuffix>,
    },
    FloatLiteral {
        value: f64,
        suffix: Option<FloatSuffix>,
    },

    // Identifier
    Identifier(String),

    // Type keywords
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    Bool,

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
    ColonColon,
    Semicolon,
    Comma,
    Arrow,
    FatArrow,
    Dot,
    DotDotEq,

    // Label
    Label(String),

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
