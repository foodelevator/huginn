use std::collections::HashMap;

use crate::{bitcode as bit, bytecode as byte, Array};

pub fn analyze_mod(module: &byte::Module) -> bit::Module {
    let mut procedures = Array::new();
    for proc in module.symbols.values() {
        procedures.push(analyze_proc(proc));
    }

    bit::Module { procedures }
}

pub fn analyze_proc(proc: &byte::Procedure) -> bit::Procedure {
    let mut ctx = AnalyzingContext::new();
    ctx.proc(proc);

    bit::Procedure {
        name: proc.name.to_string(),
        blocks: ctx.blocks,
        values: ctx.values,
        local_count: ctx.local_count,
    }
}

struct AnalyzingContext<'a> {
    scope: HashMap<&'a str, u32>,
    blocks: Array<bit::BlockId, bit::Block>,
    curr_block: bit::BlockId,
    values: HashMap<bit::Value, bit::ValueInfo>,
    local_count: u32,
}

impl<'a> AnalyzingContext<'a> {
    fn new() -> Self {
        Self {
            scope: HashMap::new(),
            blocks: Array::default(),
            curr_block: 0,
            values: HashMap::new(),
            local_count: 0,
        }
    }

    fn proc(&mut self, proc: &'a byte::Procedure) {
        for block in proc.blocks.values() {
            self.block(block)
        }
    }

    fn block(&mut self, block: &'a byte::Block) {
        self.curr_block = self.blocks.push(bit::Block { instrs: Vec::new() });
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
                    src: src.into(),
                })
            }
            byte::Instr::SymbolVal { dest, name } => {
                let src = self.scope[&name[..]];
                self.emit(bit::Instr::Mov {
                    dest: dest.into(),
                    src: bit::Value::Local(src),
                })
            }
            &byte::Instr::Const { dest, val } => self.emit(bit::Instr::Const {
                dest: dest.into(),
                val,
            }),
            &byte::Instr::BinaryOperation {
                dest,
                lhs,
                rhs,
                operator,
            } => self.emit(bit::Instr::BinaryOperation {
                dest: dest.into(),
                lhs: lhs.into(),
                rhs: rhs.into(),
                operator,
            }),
            &byte::Instr::UnaryOperation {
                dest,
                operand,
                operator,
            } => self.emit(bit::Instr::UnaryOperation {
                dest: dest.into(),
                operand: operand.into(),
                operator,
            }),
            &byte::Instr::Jump(block) => self.emit(bit::Instr::Jump(block)),
            &byte::Instr::Branch {
                cond,
                then_block,
                else_block,
            } => self.emit(bit::Instr::Branch {
                cond: cond.into(),
                then_block,
                else_block,
            }),
            &byte::Instr::Print(val) => self.emit(bit::Instr::Print(val.into())),
            &byte::Instr::Return(val) => self.emit(bit::Instr::Return(val.into())),
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
            }

            bit::Instr::Branch { .. }
            | bit::Instr::Jump(_)
            | bit::Instr::Print(_)
            | bit::Instr::Return(_) => {}
        }
        self.blocks[self.curr_block].instrs.push(instr);
    }
}

impl From<&byte::Value> for bit::Value {
    fn from(val: &byte::Value) -> Self {
        bit::Value::Temp(*val)
    }
}

impl From<byte::Value> for bit::Value {
    fn from(val: byte::Value) -> Self {
        bit::Value::Temp(val)
    }
}
