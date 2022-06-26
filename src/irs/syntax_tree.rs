use crate::common::{BinaryOperator, Ident, Span, UnaryOperator};

#[derive(Debug, Clone)]
pub struct Block {
    pub left_curly: Span,
    pub stmts: Vec<Stmt>,
    pub right_curly: Span,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    Assign(Assign),
    If(IfStmt),
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub assignee: Assignee,
    pub assign_sign: Span,
    pub value: Expr,
}

#[derive(Debug, Clone)]
pub enum Assignee {
    Expr(Expr),
    Let(Ident),
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub if_span: Span,
    pub cond: Expr,
    pub then: Block,
    pub else_: Option<(Span, Block)>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Grouping(Grouping),
    Int(Span, i64),
    BinaryOperation(Box<BinaryOperation>),
    UnaryOperation(Box<UnaryOperation>),
    If(Box<IfExpr>),
    Ident(Ident),
}

#[derive(Debug, Clone)]
pub struct Grouping {
    pub left_paren: Span,
    pub expr: Box<Expr>,
    pub right_paren: Span,
}

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub if_span: Span,
    pub cond: Expr,
    pub then_span: Span,
    pub then: Expr,
    pub else_span: Span,
    pub else_: Expr,
}

#[derive(Debug, Clone)]
pub struct BinaryOperation {
    pub lhs: Expr,
    pub rhs: Expr,
    pub op_span: Span,
    pub operator: BinaryOperator,
}

#[derive(Debug, Clone)]
pub struct UnaryOperation {
    pub op_span: Span,
    pub operand: Expr,
    pub operator: UnaryOperator,
}
