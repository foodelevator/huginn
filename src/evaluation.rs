use crate::parsing::{BinaryOperation, BinaryOperator, Expr};

pub trait Eval {
    fn eval(&self) -> i64;
}

impl Eval for Expr {
    fn eval(&self) -> i64 {
        match self {
            Expr::Grouping { expr, .. } => expr.eval(),
            Expr::Int(_, val) => *val,
            Expr::BinaryOperation(bin_op) => bin_op.eval(),
        }
    }
}

impl Eval for BinaryOperation {
    fn eval(&self) -> i64 {
        match self.operator {
            BinaryOperator::Plus => self.lhs.eval() + self.rhs.eval(),
            BinaryOperator::Times => self.lhs.eval() * self.rhs.eval(),
        }
    }
}
