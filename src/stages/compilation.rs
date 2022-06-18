use crate::{
    bytecode::{Instr, Value},
    syntax_tree::{BinaryOperation, Expr, Grouping},
};

#[derive(Debug, Default)]
pub struct Compilation {
    instrs: Vec<Instr>,
    var_counter: Value,
}

impl Compilation {
    pub fn compile(expr: &Expr) -> Vec<Instr> {
        let mut this = Self::default();
        this.compile_expr(expr);
        this.instrs
    }

    fn compile_expr(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::Grouping(Grouping { expr, .. }) => self.compile_expr(expr),
            &Expr::Int(_, val) => {
                let dest = self.var();
                self.emit(Instr::Const { dest, val });
                dest
            }
            Expr::BinaryOperation(bin_op) => self.compile_bin_op(bin_op),
        }
    }

    fn compile_bin_op(&mut self, bin_op: &BinaryOperation) -> Value {
        let l = self.compile_expr(&bin_op.lhs);
        let r = self.compile_expr(&bin_op.rhs);
        let dest = self.var();
        self.emit(Instr::BinOp {
            dest,
            lhs: l,
            rhs: r,
            operator: bin_op.operator,
        });
        dest
    }

    fn emit(&mut self, instr: Instr) {
        self.instrs.push(instr)
    }

    fn var(&mut self) -> Value {
        let res = self.var_counter;
        self.var_counter += 1;
        res
    }
}
