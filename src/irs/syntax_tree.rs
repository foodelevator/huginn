use crate::common::{BinaryOperator, Span, UnaryOperator};

#[derive(Debug, Clone)]
pub enum Expr {
    Grouping(Grouping),
    Int(Span, i64),
    BinaryOperation(Box<BinaryOperation>),
    UnaryOperation(Box<UnaryOperation>),
    If(Box<If>),
}

#[derive(Debug, Clone)]
pub struct Grouping {
    pub left_paren: Span,
    pub expr: Box<Expr>,
    pub right_paren: Span,
}

#[derive(Debug, Clone)]
pub struct If {
    pub if_span: Span,
    pub cond: Expr,
    pub then: Expr,  // TODO: make Block
    pub else_: Expr, // TODO: make Option<Block>
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
