use crate::parsing::{BinaryOperation, BinaryOperator, Expr};

pub trait Eval {
    fn eval(&self) -> usize;
}

impl Eval for Expr {
    fn eval(&self) -> usize {
        match self {
            Expr::Int(_, val) => *val,
            Expr::BinaryOperation(bin_op) => bin_op.eval(),
            Expr::Grouping { expr, .. } => expr.eval(),
        }
    }
}

impl Eval for BinaryOperation {
    fn eval(&self) -> usize {
        match self.operator {
            BinaryOperator::Plus => self.lhs.eval() + self.rhs.eval(),
            BinaryOperator::Times => self.lhs.eval() * self.rhs.eval(),
        }
    }
}
