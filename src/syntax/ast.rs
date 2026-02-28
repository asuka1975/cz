use crate::arena::Id;
use crate::diagnostics::Span;
use crate::syntax::token::{FloatSuffix, IntSuffix};

pub type ExprId = Id<Expr>;
pub type StmtId = Id<Stmt>;

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<FunctionDef>,
    pub structs: Vec<StructDef>,
    pub enums: Vec<EnumDef>,
}

#[derive(Debug)]
pub struct FunctionDef {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
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
    pub stmts: Vec<StmtId>,
    pub expr: Option<ExprId>,
}

#[derive(Debug)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<FieldDef>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FieldDef {
    pub name: String,
    pub field_type: Type,
}

#[derive(Debug)]
pub struct EnumDef {
    pub name: String,
    pub variants: Vec<VariantDef>,
    pub span: Span,
}

#[derive(Debug)]
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
pub enum Stmt {
    Let {
        name: String,
        mutable: bool,
        var_type: Option<Type>,
        init: ExprId,
        span: Span,
    },
    Return {
        value: Option<ExprId>,
        span: Span,
    },
    Break {
        label: Option<String>,
        value: Option<ExprId>,
        span: Span,
    },
    Continue {
        label: Option<String>,
        span: Span,
    },
    Expr(ExprId),
}

#[derive(Debug)]
#[allow(clippy::enum_variant_names)]
pub enum Expr {
    IntegerLiteral {
        value: i64,
        suffix: Option<IntSuffix>,
        span: Span,
    },
    FloatLiteral {
        value: f64,
        suffix: Option<FloatSuffix>,
        span: Span,
    },
    BoolLiteral {
        value: bool,
        span: Span,
    },
    UnitLiteral {
        span: Span,
    },
    Identifier {
        name: String,
        span: Span,
    },
    BinaryOp {
        op: BinOp,
        left: ExprId,
        right: ExprId,
        span: Span,
    },
    UnaryOp {
        op: UnaryOp,
        operand: ExprId,
        span: Span,
    },
    Cast {
        expr: ExprId,
        target_type: Type,
        span: Span,
    },
    Call {
        name: String,
        args: Vec<ExprId>,
        span: Span,
    },
    Assign {
        name: String,
        value: ExprId,
        span: Span,
    },
    If {
        condition: ExprId,
        then_block: Block,
        else_block: Option<ElseClause>,
        span: Span,
    },
    While {
        label: Option<String>,
        condition: ExprId,
        body: Block,
        span: Span,
    },
    Match {
        expr: ExprId,
        arms: Vec<MatchArm>,
        span: Span,
    },
    Block {
        block: Block,
        span: Span,
    },
    FieldAccess {
        expr: ExprId,
        field: String,
        span: Span,
    },
    TupleIndex {
        expr: ExprId,
        index: u32,
        span: Span,
    },
    TupleExpr {
        elements: Vec<ExprId>,
        span: Span,
    },
    StructExpr {
        name: String,
        fields: Vec<(String, ExprId)>,
        span: Span,
    },
    EnumExpr {
        enum_name: String,
        variant: String,
        args: EnumArgs,
        span: Span,
    },
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::IntegerLiteral { span, .. }
            | Expr::FloatLiteral { span, .. }
            | Expr::BoolLiteral { span, .. }
            | Expr::UnitLiteral { span }
            | Expr::Identifier { span, .. }
            | Expr::BinaryOp { span, .. }
            | Expr::UnaryOp { span, .. }
            | Expr::Cast { span, .. }
            | Expr::Call { span, .. }
            | Expr::Assign { span, .. }
            | Expr::If { span, .. }
            | Expr::While { span, .. }
            | Expr::Match { span, .. }
            | Expr::Block { span, .. }
            | Expr::FieldAccess { span, .. }
            | Expr::TupleIndex { span, .. }
            | Expr::TupleExpr { span, .. }
            | Expr::StructExpr { span, .. }
            | Expr::EnumExpr { span, .. } => *span,
        }
    }

    pub fn ends_with_block(&self) -> bool {
        matches!(
            self,
            Expr::If { .. }
                | Expr::Block { .. }
                | Expr::Assign { .. }
                | Expr::While { .. }
                | Expr::Match { .. }
        )
    }
}

#[derive(Debug)]
pub enum EnumArgs {
    Unit,
    Tuple(Vec<ExprId>),
    Struct(Vec<(String, ExprId)>),
}

#[derive(Debug)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: ExprId,
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
    ElseIf(ExprId),
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
