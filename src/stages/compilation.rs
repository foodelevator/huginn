use std::collections::HashMap;

use crate::{
    bytecode::{Block, BlockId, Function, Instr, Value},
    syntax_tree::{Assign, Assignee, BinaryOperation, Expr, Grouping, If, Stmt, UnaryOperation},
};

pub fn compile(stmts: &[Stmt]) -> Function {
    assert!(!stmts.is_empty());

    let mut compiler = Compiler::new();
    for stmt in &stmts[..stmts.len() - 1] {
        compiler.stmt(stmt);
    }
    let ret = compiler.stmt(&stmts[stmts.len() - 1]);
    compiler.emit(Instr::Return(ret));
    Function {
        blocks: compiler.blocks,
    }
}

#[derive(Debug)]
struct Compiler {
    blocks: Vec<Block>,
    curr_block: BlockId,
    var_counter: Value,
    scope: HashMap<String, Value>,
}

impl Compiler {
    fn new() -> Self {
        let mut this = Self {
            blocks: Vec::new(),
            curr_block: 0,
            var_counter: 0,
            scope: HashMap::new(),
        };
        this.create_block();
        this
    }

    // TODO: this return value should just be a temporary hack. Statements don't evaluate to
    // values.
    fn stmt(&mut self, stmt: &Stmt) -> Value {
        match stmt {
            Stmt::Expr(expr) => {
                let dest = self.var();
                self.rval(expr, dest);
                dest
            }
            Stmt::Assign(Assign {
                assignee, value, ..
            }) => {
                let dest = match assignee {
                    Assignee::Expr(expr) => self.lval(expr),
                    Assignee::Let(ident) => {
                        let var = self.var();
                        self.scope.insert(ident.name.clone(), var);
                        var
                    }
                };
                self.rval(value, dest);
                dest
            }
        }
    }

    fn lval(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::Grouping(Grouping { expr, .. }) => self.lval(expr),
            Expr::Int(_, _) | Expr::BinaryOperation(_) | Expr::If(_) | Expr::UnaryOperation(_) => {
                // TODO: fix error reporting
                panic!("Expression {:?} is not an lvalue", expr)
            }
            Expr::Ident(ident) => *self.scope.get(&ident.name).expect("Unkown symbol"),
            Expr::Error => unreachable!(),
        }
    }

    fn rval(&mut self, expr: &Expr, dest: Value) {
        match expr {
            Expr::Grouping(Grouping { expr, .. }) => self.rval(expr, dest),
            &Expr::Int(_, val) => {
                self.emit(Instr::Const { dest, val });
            }
            Expr::BinaryOperation(bin_op) => self.bin_op(bin_op, dest),
            Expr::UnaryOperation(bin_op) => self.unary_op(bin_op, dest),
            Expr::If(if_) => self.if_(if_, dest),
            Expr::Ident(ident) => {
                let src = *self.scope.get(&ident.name).expect("Unknown symbol");
                self.emit(Instr::Mov { dest, src })
            }
            Expr::Error => unreachable!(),
        }
    }

    fn bin_op(&mut self, bin_op: &BinaryOperation, dest: Value) {
        let lhs = self.var();
        self.rval(&bin_op.lhs, lhs);
        let rhs = self.var();
        self.rval(&bin_op.rhs, rhs);
        self.emit(Instr::BinaryOperator {
            dest,
            lhs,
            rhs,
            operator: bin_op.operator,
        });
    }

    fn unary_op(&mut self, unary_op: &UnaryOperation, dest: Value) {
        let operand = self.var();
        self.rval(&unary_op.operand, operand);
        let operator = unary_op.operator;
        self.emit(Instr::UnaryOperator {
            dest,
            operand,
            operator,
        });
    }

    fn if_(&mut self, if_: &If, dest: Value) {
        let cond = self.var();
        self.rval(&if_.cond, cond);
        let then = self.create_block();
        let else_ = self.create_block();
        let done = self.create_block();
        self.emit(Instr::JumpIf {
            cond,
            block_id: then,
        });
        self.emit(Instr::Jump(else_));

        self.switch_to_block(then);
        self.rval(&if_.then, dest);
        self.emit(Instr::Jump(done));

        self.switch_to_block(else_);
        self.rval(&if_.else_, dest);
        self.emit(Instr::Jump(done));

        self.switch_to_block(done);
    }

    fn emit(&mut self, instr: Instr) {
        self.blocks[self.curr_block as usize].instrs.push(instr)
    }

    fn switch_to_block(&mut self, id: BlockId) {
        debug_assert!((id as usize) < self.blocks.len());
        self.curr_block = id;
    }

    fn create_block(&mut self) -> BlockId {
        let id = self.blocks.len() as BlockId;
        self.blocks.push(Block::default());
        id
    }

    fn var(&mut self) -> Value {
        let res = self.var_counter;
        self.var_counter += 1;
        res
    }
}
