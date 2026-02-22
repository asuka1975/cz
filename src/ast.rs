#[derive(Debug)]
pub struct Program {
    pub functions: Vec<FunctionDef>,
}

#[derive(Debug)]
pub struct FunctionDef {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub body: Block,
    pub line: usize,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub param_type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    I32,
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub expr: Option<Box<Expr>>,
}

#[derive(Debug)]
pub enum Stmt {
    Let {
        name: String,
        mutable: bool,
        var_type: Type,
        init: Expr,
        line: usize,
    },
    While {
        condition: Expr,
        body: Block,
    },
    Return {
        value: Expr,
        line: usize,
    },
    Expr(Expr),
}

#[derive(Debug)]
pub enum Expr {
    IntegerLiteral(i64),
    BoolLiteral(bool),
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
    Block(Block),
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
