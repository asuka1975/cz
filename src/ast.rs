use crate::token::{FloatSuffix, IntSuffix};

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<FunctionDef>,
    pub structs: Vec<StructDef>,
    pub enums: Vec<EnumDef>,
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct FunctionDef {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub body: Block,
    pub line: usize,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct Param {
    pub name: String,
    pub param_type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    Bool,
    Unit,
    Tuple(Vec<Type>),
    Named(String),
    /// 型推論に失敗した場合のエラー型。カスケードエラー防止に使用する。
    Error,
}

impl Type {
    pub fn is_integer(&self) -> bool {
        matches!(self, Type::I8 | Type::I16 | Type::I32 | Type::I64)
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Type::F32 | Type::F64)
    }

    pub fn is_numeric(&self) -> bool {
        self.is_integer() || self.is_float()
    }
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub expr: Option<Box<Expr>>,
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<FieldDef>,
    pub line: usize,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct FieldDef {
    pub name: String,
    pub field_type: Type,
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct EnumDef {
    pub name: String,
    pub variants: Vec<VariantDef>,
    pub line: usize,
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct VariantDef {
    pub name: String,
    pub kind: VariantKind,
}

#[derive(Debug, Clone)]
pub enum VariantKind {
    Unit,
    Tuple(Vec<Type>),
    Struct(Vec<FieldDef>),
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum Stmt {
    Let {
        name: String,
        mutable: bool,
        var_type: Option<Type>,
        init: Expr,
        line: usize,
    },
    Return {
        value: Option<Expr>,
        line: usize,
    },
    Break {
        label: Option<String>,
        value: Option<Expr>,
        line: usize,
    },
    Continue {
        label: Option<String>,
        line: usize,
    },
    Expr(Expr),
}

#[derive(Debug)]
#[allow(clippy::enum_variant_names)]
pub enum Expr {
    IntegerLiteral {
        value: i64,
        suffix: Option<IntSuffix>,
    },
    FloatLiteral {
        value: f64,
        suffix: Option<FloatSuffix>,
    },
    BoolLiteral(bool),
    UnitLiteral,
    Identifier(String, usize),
    BinaryOp {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    UnaryOp {
        op: UnaryOp,
        operand: Box<Expr>,
    },
    Cast {
        expr: Box<Expr>,
        target_type: Type,
    },
    Call {
        name: String,
        args: Vec<Expr>,
        line: usize,
    },
    Assign {
        name: String,
        value: Box<Expr>,
        line: usize,
    },
    If {
        condition: Box<Expr>,
        then_block: Block,
        else_block: Option<Box<ElseClause>>,
    },
    While {
        label: Option<String>,
        condition: Box<Expr>,
        body: Block,
    },
    Match {
        expr: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    Block(Block),
    FieldAccess {
        expr: Box<Expr>,
        field: String,
    },
    TupleIndex {
        expr: Box<Expr>,
        index: u32,
    },
    TupleExpr(Vec<Expr>),
    StructExpr {
        name: String,
        fields: Vec<(String, Expr)>,
        line: usize,
    },
    EnumExpr {
        enum_name: String,
        variant: String,
        args: EnumArgs,
        line: usize,
    },
}

#[derive(Debug)]
pub enum EnumArgs {
    Unit,
    Tuple(Vec<Expr>),
    Struct(Vec<(String, Expr)>),
}

#[derive(Debug)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Expr,
}

#[derive(Debug)]
pub enum Pattern {
    IntLiteral(i64),
    BoolLiteral(bool),
    Range {
        start: i64,
        end: i64,
    },
    Wildcard,
    Binding(String),
    Tuple(Vec<Pattern>),
    Struct {
        name: String,
        fields: Vec<(String, Pattern)>,
    },
    Enum {
        enum_name: String,
        variant: String,
        args: EnumPatternArgs,
    },
}

#[derive(Debug)]
pub enum EnumPatternArgs {
    Unit,
    Tuple(Vec<Pattern>),
    Struct(Vec<(String, Pattern)>),
}

#[derive(Debug)]
pub enum ElseClause {
    ElseBlock(Block),
    ElseIf(Box<Expr>),
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    And,
    Or,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Neg,
    Not,
}
