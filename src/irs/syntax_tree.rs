use crate::common::{BinaryOperator, Span};

#[derive(Debug, Clone)]
pub enum Expr {
    Grouping {
        left_paren: Span,
        expr: Box<Expr>,
        right_paren: Span,
    },
    Int(Span, i64),
    BinaryOperation(Box<BinaryOperation>),
}

#[derive(Debug, Clone)]
pub struct BinaryOperation {
    pub lhs: Expr,
    pub rhs: Expr,
    pub op_span: Span,
    pub operator: BinaryOperator,
}
