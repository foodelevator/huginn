use std::collections::HashMap;

use crate::{
    bytecode::{Block as BCBlock, BlockId, Instr, Module, Procedure, Value},
    common::Ident,
    syntax_tree::{
        Assign, BinaryOperation, Block, Expr, ExprStmt, File, Grouping, IfExpr, IfStmt, Proc, Stmt,
        UnaryOperation, VarDecl, While,
    },
    Array,
};

pub fn lower_file(file: &File) -> Module {
    let mut procedures = Array::new();
    let mut scope = HashMap::new();
    for stmt in &file.stmts {
        match stmt {
            Stmt::VarDecl(VarDecl {
                ident,
                value: Expr::Proc(proc),
                ..
            }) => {
                scope.insert(ident.name.clone(), procedures.len());
                procedures.push(lower_proc(proc));
            }

            Stmt::Expr(_)
            | Stmt::VarDecl(_)
            | Stmt::Assign(_)
            | Stmt::If(_)
            | Stmt::While(_)
            | Stmt::Print(_, _, _)
            | Stmt::Return(_, _, _) => panic!(),
        }
    }

    Module { procedures, scope }
}

pub fn lower_proc(proc: &Proc) -> Procedure {
    let mut ctx = LoweringContext::new();
    for stmt in &proc.body.stmts {
        ctx.stmt(stmt);
    }

    // Since we at the moment always must return a value:
    let ret = ctx.var();
    ctx.emit(Instr::Const { dest: ret, val: 0 });
    ctx.emit(Instr::Return(ret));

    Procedure { blocks: ctx.blocks }
}

pub fn lower_stmt(stmt: &Stmt) -> Procedure {
    let mut ctx = LoweringContext::new();
    ctx.stmt(stmt);

    // Since we at the moment always must return a value:
    let ret = ctx.var();
    ctx.emit(Instr::Const { dest: ret, val: 0 });
    ctx.emit(Instr::Return(ret));

    Procedure { blocks: ctx.blocks }
}

pub fn lower_expr(expr: &Expr) -> Procedure {
    let mut ctx = LoweringContext::new();
    let ret = ctx.var();
    ctx.rval(expr, ret);
    ctx.emit(Instr::Return(ret));

    Procedure { blocks: ctx.blocks }
}

#[derive(Debug)]
struct LoweringContext {
    blocks: Array<BlockId, BCBlock>,
    curr_block: BlockId,
    var_counter: Value,
}

impl LoweringContext {
    fn new() -> Self {
        let mut this = Self {
            blocks: Array::default(),
            curr_block: 0,
            var_counter: 0,
        };
        this.create_block();
        this
    }

    fn block(&mut self, block: &Block) {
        for stmt in &block.stmts {
            self.stmt(stmt);
        }
    }

    fn stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(ExprStmt { expr, .. }) => {
                let dest = self.var();
                self.rval(expr, dest);
            }
            Stmt::VarDecl(VarDecl { ident, value, .. }) => {
                let src = self.var();
                self.rval(value, src);

                self.emit(Instr::DefLocal {
                    name: ident.name.clone(),
                });
                self.emit(Instr::SymAssign {
                    name: ident.name.clone(),
                    src,
                });
            }
            Stmt::Assign(Assign {
                assignee, value, ..
            }) => {
                let src = self.var();
                self.rval(value, src);
                self.assign(assignee, src);
            }
            Stmt::If(if_) => {
                self.if_stmt(if_);
            }
            Stmt::While(while_) => {
                self.while_(while_);
            }
            Stmt::Print(_, expr, _) => {
                let var = self.var();
                self.rval(expr, var);
                self.emit(Instr::Print(var))
            }
            Stmt::Return(_, expr, _) => {
                let var = self.var();
                self.rval(expr, var);
                self.emit(Instr::Return(var));
            }
        }
    }

    fn assign(&mut self, assignee: &Expr, src: Value) {
        match assignee {
            Expr::Grouping(_)
            | Expr::Int(_, _)
            | Expr::BinaryOperation(_)
            | Expr::UnaryOperation(_)
            | Expr::If(_)
            | Expr::Proc(_) => panic!(),
            Expr::Ident(Ident { name, .. }) => {
                let name = name.clone();
                self.emit(Instr::SymAssign { name, src });
            }
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
            Expr::If(if_) => self.if_expr(if_, dest),
            Expr::Ident(Ident { name, .. }) => self.emit(Instr::SymbolVal {
                dest,
                name: name.clone(),
            }),
            Expr::Proc(_) => panic!(),
        }
    }

    fn bin_op(&mut self, bin_op: &BinaryOperation, dest: Value) {
        let lhs = self.var();
        self.rval(&bin_op.lhs, lhs);
        let rhs = self.var();
        self.rval(&bin_op.rhs, rhs);
        self.emit(Instr::BinaryOperation {
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
        self.emit(Instr::UnaryOperation {
            dest,
            operand,
            operator,
        });
    }

    fn if_stmt(&mut self, if_: &IfStmt) {
        let cond = self.var();
        self.rval(&if_.cond, cond);
        let then = self.create_block();
        let else_ = self.create_block();
        let done = self.create_block();
        self.emit(Instr::Branch {
            cond,
            then_block: then,
            else_block: else_,
        });

        self.switch_to_block(then);
        self.block(&if_.then);
        self.emit(Instr::Jump(done));

        self.switch_to_block(else_);
        if let Some(else_) = &if_.else_ {
            self.block(&else_.1);
        }
        self.emit(Instr::Jump(done));

        self.switch_to_block(done);
    }

    fn while_(&mut self, while_: &While) {
        let cond_block = self.create_block();
        let body = self.create_block();
        let done = self.create_block();
        self.emit(Instr::Jump(body));

        self.switch_to_block(cond_block);

        let cond = self.var();
        self.rval(&while_.cond, cond);
        self.emit(Instr::Branch {
            cond,
            then_block: body,
            else_block: done,
        });

        self.switch_to_block(body);

        self.block(&while_.body);
        self.emit(Instr::Jump(cond_block));

        self.switch_to_block(done);
    }

    fn if_expr(&mut self, if_: &IfExpr, dest: Value) {
        let cond = self.var();
        self.rval(&if_.cond, cond);
        let then = self.create_block();
        let else_ = self.create_block();
        let done = self.create_block();
        self.emit(Instr::Branch {
            cond,
            then_block: then,
            else_block: else_,
        });

        self.switch_to_block(then);
        self.rval(&if_.then, dest);
        self.emit(Instr::Jump(done));

        self.switch_to_block(else_);
        self.rval(&if_.else_, dest);
        self.emit(Instr::Jump(done));

        self.switch_to_block(done);
    }

    fn emit(&mut self, instr: Instr) {
        self.blocks[self.curr_block].instrs.push(instr)
    }

    fn switch_to_block(&mut self, id: BlockId) {
        debug_assert!(id < self.blocks.len());
        self.curr_block = id;
    }

    fn create_block(&mut self) -> BlockId {
        let id = self.blocks.len() as BlockId;
        self.blocks.push(BCBlock::default());
        id
    }

    fn var(&mut self) -> Value {
        let res = self.var_counter;
        self.var_counter += 1;
        res
    }
}
