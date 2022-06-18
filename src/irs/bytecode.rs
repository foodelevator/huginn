use crate::common::BinaryOperator;

#[derive(Debug, Default)]
pub struct Function {
    pub blocks: Vec<Block>,
}

#[derive(Debug, Default)]
pub struct Block {
    pub instrs: Vec<Instr>,
}

#[derive(Debug, Clone, Copy)]
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
    Mov {
        dest: Value,
        src: Value,
    },
    Jump(BlockId),
    JumpIf {
        cond: Value,
        block_id: BlockId,
    },
    Return(Value),
}

pub type BlockId = u32;
pub type Value = u32;
