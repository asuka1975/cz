pub mod lower;
pub mod types;

use crate::arena::{Arena, Id};
use crate::diagnostics::Span;
use crate::syntax::ast::{BinOp, Pattern, UnaryOp};

pub use types::Type;

pub type HirExprId = Id<HirExpr>;
pub type HirStmtId = Id<HirStmt>;

/// 変数を一意に識別する ID。
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct VarId(pub u32);

/// 関数を一意に識別する ID。
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FuncId(pub u32);

/// 変数の情報 (VarId → VarInfo で参照)。
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct VarInfo {
    pub name: String,
    pub ty: Type,
    pub mutable: bool,
    pub span: Span,
}

/// HIR 式ノード。型 + Span が付随する。
#[derive(Debug)]
pub struct HirExpr {
    pub kind: HirExprKind,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug)]
pub enum HirExprKind {
    IntegerLiteral {
        value: i64,
    },
    FloatLiteral {
        value: f64,
    },
    BoolLiteral(bool),
    UnitLiteral,
    Var(VarId),
    BinaryOp {
        op: BinOp,
        left: HirExprId,
        right: HirExprId,
    },
    UnaryOp {
        op: UnaryOp,
        operand: HirExprId,
    },
    Cast {
        expr: HirExprId,
        target_type: Type,
    },
    Call {
        func: FuncId,
        args: Vec<HirExprId>,
    },
    Assign {
        var: VarId,
        value: HirExprId,
    },
    If {
        condition: HirExprId,
        then_block: HirBlock,
        else_block: Option<HirElseClause>,
    },
    While {
        label: Option<String>,
        condition: HirExprId,
        body: HirBlock,
    },
    Match {
        expr: HirExprId,
        arms: Vec<HirMatchArm>,
    },
    Block(HirBlock),
    FieldAccess {
        expr: HirExprId,
        struct_name: String,
        field_index: usize,
    },
    TupleIndex {
        expr: HirExprId,
        index: u32,
    },
    TupleExpr(Vec<HirExprId>),
    StructExpr {
        name: String,
        fields: Vec<HirExprId>,
    },
    EnumExpr {
        enum_name: String,
        variant_index: usize,
        args: HirEnumArgs,
    },
}

#[derive(Debug)]
pub struct HirBlock {
    pub stmts: Vec<HirStmtId>,
    pub expr: Option<HirExprId>,
}

#[derive(Debug)]
pub enum HirStmt {
    Let {
        var: VarId,
        init: HirExprId,
        span: Span,
    },
    Return {
        value: Option<HirExprId>,
        span: Span,
    },
    Break {
        label: Option<String>,
        value: Option<HirExprId>,
        span: Span,
    },
    Continue {
        label: Option<String>,
        span: Span,
    },
    Expr(HirExprId),
}

#[derive(Debug)]
pub enum HirElseClause {
    ElseBlock(HirBlock),
    ElseIf(HirExprId),
}

#[derive(Debug)]
pub struct HirMatchArm {
    pub pattern: Pattern,
    pub body: HirExprId,
}

#[derive(Debug)]
pub enum HirEnumArgs {
    Unit,
    Tuple(Vec<HirExprId>),
    Struct(Vec<HirExprId>),
}

/// HIR プログラム全体。
pub struct HirProgram {
    pub functions: Vec<HirFunctionDef>,
    pub struct_order: Vec<String>,
    pub enum_order: Vec<String>,
}

pub struct HirFunctionDef {
    pub func_id: FuncId,
    pub name: String,
    pub params: Vec<VarId>,
    pub return_type: Type,
    pub body: HirBlock,
    pub span: Span,
}

/// Lowering の出力。
pub struct LowerResult {
    pub program: HirProgram,
    pub expr_arena: Arena<HirExpr>,
    pub stmt_arena: Arena<HirStmt>,
    pub vars: Vec<VarInfo>,
    pub func_names: Vec<String>,
}
