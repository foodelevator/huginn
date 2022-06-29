use std::collections::HashMap;

use crate::{
    bytecode::{Block as BCBlock, BlockId, Function, Instr, Value},
    syntax_tree::{
        Assign, Assignee, BinaryOperation, Block, Expr, ExprStmt, Grouping, IfExpr, IfStmt, Stmt,
        UnaryOperation,
    },
};

pub fn compile_block(block: &Block) -> Function {
    let mut compiler = Compiler::new();
    compiler.block(block);

    // Since we at the moment always must return a value:
    let ret = compiler.var();
    compiler.emit(Instr::Const { dest: ret, val: 0 });
    compiler.emit(Instr::Return(ret));

    Function {
        blocks: compiler.blocks,
    }
}

pub fn compile_stmt(stmt: &Stmt, scope: &HashMap<String, i64>) -> Function {
    let mut compiler = Compiler::new();
    for (name, &val) in scope {
        let dest = compiler.var();
        compiler.emit(Instr::Const { dest, val });
        compiler.scope.insert(name.to_string(), dest);
    }
    compiler.stmt(stmt);

    // Since we at the moment always must return a value:
    let ret = compiler.var();
    compiler.emit(Instr::Const { dest: ret, val: 0 });
    compiler.emit(Instr::Return(ret));

    Function {
        blocks: compiler.blocks,
    }
}

pub fn compile_expr(expr: &Expr, scope: &HashMap<String, i64>) -> Function {
    let mut compiler = Compiler::new();
    for (name, &val) in scope {
        let dest = compiler.var();
        compiler.emit(Instr::Const { dest, val });
        compiler.scope.insert(name.to_string(), dest);
    }

    let ret = compiler.var();
    compiler.rval(expr, ret);
    compiler.emit(Instr::Return(ret));

    Function {
        blocks: compiler.blocks,
    }
}

#[derive(Debug)]
struct Compiler {
    blocks: Vec<BCBlock>,
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
            Stmt::Assign(Assign {
                assignee, value, ..
            }) => {
                let (dest, ident) = match assignee {
                    Assignee::Expr(expr) => (self.lval(expr), None),
                    Assignee::Let(ident) => {
                        let var = self.var();
                        (var, Some(ident))
                    }
                };
                self.rval(value, dest);
                // This must be done after `self.rval` since we allow variable shadowing and
                // `value` might use an old variable with the same name as the one we're defining
                // here.
                if let Some(ident) = ident {
                    self.scope.insert(ident.name.clone(), dest);
                }
            }
            Stmt::If(if_stmt) => {
                self.if_stmt(if_stmt);
            }
            Stmt::Print(_, grouping, _) => {
                let var = self.var();
                self.rval(&grouping.expr, var);
                self.emit(Instr::Print(var))
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
            Expr::Ident(ident) => {
                let src = *self.scope.get(&ident.name).expect("Unknown symbol");
                self.emit(Instr::Mov { dest, src })
            }
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

    fn if_stmt(&mut self, if_: &IfStmt) {
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
        self.block(&if_.then);
        self.emit(Instr::Jump(done));

        self.switch_to_block(else_);
        if let Some(else_) = &if_.else_ {
            self.block(&else_.1);
        }
        self.emit(Instr::Jump(done));

        self.switch_to_block(done);
    }

    fn if_expr(&mut self, if_: &IfExpr, dest: Value) {
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
        self.blocks.push(BCBlock::default());
        id
    }

    fn var(&mut self) -> Value {
        let res = self.var_counter;
        self.var_counter += 1;
        res
    }
}
