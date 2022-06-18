use crate::common::BinaryOperator;

#[derive(Debug)]
pub enum Instr {
    Const {
        dest: Value,
        val: i64,
    },
    BinOp {
        dest: Value,
        lhs: Value,
        rhs: Value,
        operator: BinaryOperator,
    },
}

pub type Value = u32;
