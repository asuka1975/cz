/// Top-level program node containing all declarations.
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}

/// Top-level declarations in the cz language.
#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Function(FunctionDecl),
}

/// A function declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDecl {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub body: Block,
}

/// A function parameter.
#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: String,
    pub ty: Type,
}

/// A block of statements.
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

/// A statement.
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Return(Option<Expr>),
    Expr(Expr),
}

/// An expression.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Integer(i64),
    Identifier(String),
    BinaryOp {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Call {
        callee: String,
        args: Vec<Expr>,
    },
}

/// Binary operators.
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

/// Types in the cz language.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Void,
    Named(String),
}
