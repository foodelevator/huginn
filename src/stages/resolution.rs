use std::collections::HashMap;

use crate::{bitcode as bit, bytecode as byte};

// bytecode -> bitcode
// resolve names, types (haha), check which values are SSA, etc

pub fn resolve(proc: &byte::Procedure) -> bit::Procedure {
    let mut r = Resolver::new();

    r.proc(proc);

    bit::Procedure {
        blocks: r.blocks,
        values: r.values,
        local_count: r.local_count,
    }
}

struct Resolver<'a> {
    scope: HashMap<&'a str, u32>,
    blocks: Vec<bit::Block>,
    curr_block: bit::BlockId,
    values: HashMap<bit::Value, bit::ValueInfo>,
    local_count: u32,
}

impl<'a> Resolver<'a> {
    fn new() -> Self {
        Self {
            scope: HashMap::new(),
            blocks: Vec::new(),
            curr_block: 0,
            values: HashMap::new(),
            local_count: 0,
        }
    }

    fn proc(&mut self, proc: &'a byte::Procedure) {
        for block in &proc.blocks {
            self.block(block)
        }
    }

    fn block(&mut self, block: &'a byte::Block) {
        self.curr_block = self.blocks.len() as bit::BlockId;
        self.blocks.push(bit::Block { instrs: Vec::new() });
        for instr in &block.instrs {
            self.instr(instr)
        }
    }

    fn instr(&mut self, instr: &'a byte::Instr) {
        match instr {
            byte::Instr::DefLocal { name } => {
                self.scope.insert(name, self.scope.len() as u32);
                self.local_count += 1;
            }
            byte::Instr::SymAssign { name, src } => {
                let dest = self.scope[&name[..]];
                self.emit(bit::Instr::Mov {
                    dest: bit::Value::Local(dest),
                    src: temp(*src),
                })
            }
            byte::Instr::SymbolVal { dest, name } => {
                let src = self.scope[&name[..]];
                self.emit(bit::Instr::Mov {
                    dest: temp(*dest),
                    src: bit::Value::Local(src),
                })
            }
            &byte::Instr::Const { dest, val } => self.emit(bit::Instr::Const {
                dest: temp(dest),
                val,
            }),
            &byte::Instr::BinaryOperation {
                dest,
                lhs,
                rhs,
                operator,
            } => self.emit(bit::Instr::BinaryOperation {
                dest: temp(dest),
                lhs: temp(lhs),
                rhs: temp(rhs),
                operator,
            }),
            &byte::Instr::UnaryOperation {
                dest,
                operand,
                operator,
            } => self.emit(bit::Instr::UnaryOperation {
                dest: temp(dest),
                operand: temp(operand),
                operator,
            }),
            &byte::Instr::Jump(block) => self.emit(bit::Instr::Jump(block)),
            &byte::Instr::Branch {
                cond,
                then_block,
                else_block,
            } => self.emit(bit::Instr::Branch {
                cond: temp(cond),
                then_block,
                else_block,
            }),
            &byte::Instr::Print(val) => self.emit(bit::Instr::Print(temp(val))),
            &byte::Instr::Return(val) => self.emit(bit::Instr::Return(temp(val))),
        }
    }

    fn emit(&mut self, instr: bit::Instr) {
        match instr {
            bit::Instr::Const { dest, .. }
            | bit::Instr::Mov { dest, .. }
            | bit::Instr::BinaryOperation { dest, .. }
            | bit::Instr::UnaryOperation { dest, .. } => {
                let info = self.values.entry(dest).or_default();
                info.writes += 1;
            },

            bit::Instr::Branch { .. }
            | bit::Instr::Jump(_)
            | bit::Instr::Print(_)
            | bit::Instr::Return(_) => {}
        }
        self.blocks[self.curr_block as usize].instrs.push(instr);
    }
}

fn temp(val: byte::Value) -> bit::Value {
    bit::Value::Temp(val)
}
