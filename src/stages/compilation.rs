use crate::{
    bytecode::{Block, BlockId, Function, Instr, Value},
    syntax_tree::{BinaryOperation, Expr, Grouping, If, UnaryOperation},
};

pub fn compile(expr: &Expr) -> Function {
    let mut compiler = Compiler::new();
    let res = compiler.compile_expr(expr);
    compiler.emit(Instr::Return(res));
    Function {
        blocks: compiler.blocks,
    }
}

#[derive(Debug)]
struct Compiler {
    blocks: Vec<Block>,
    curr_block: BlockId,
    var_counter: Value,
}

impl Compiler {
    fn new() -> Self {
        let mut this = Self {
            blocks: Vec::new(),
            curr_block: 0,
            var_counter: 0,
        };
        this.create_block();
        this
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
            Expr::UnaryOperation(bin_op) => self.compile_unary_op(bin_op),
            Expr::If(if_) => self.compile_if(if_),
        }
    }

    fn compile_bin_op(&mut self, bin_op: &BinaryOperation) -> Value {
        let dest = self.var();
        let lhs = self.compile_expr(&bin_op.lhs);
        let rhs = self.compile_expr(&bin_op.rhs);
        self.emit(Instr::BinaryOperator {
            dest,
            lhs,
            rhs,
            operator: bin_op.operator,
        });
        dest
    }

    fn compile_unary_op(&mut self, unary_op: &UnaryOperation) -> Value {
        let dest = self.var();
        let operand = self.compile_expr(&unary_op.operand);
        let operator = unary_op.operator;
        self.emit(Instr::UnaryOperator {
            dest,
            operand,
            operator,
        });
        dest
    }

    fn compile_if(&mut self, if_: &If) -> Value {
        let cond = self.compile_expr(&if_.cond);
        let then = self.create_block();
        let else_ = self.create_block();
        let done = self.create_block();
        let dest = self.var();
        self.emit(Instr::JumpIf {
            cond,
            block_id: then,
        });
        self.emit(Instr::Jump(else_));

        self.switch_to_block(then);
        let res = self.compile_expr(&if_.then);
        self.emit(Instr::Mov { dest, src: res });
        self.emit(Instr::Jump(done));

        self.switch_to_block(else_);
        let res = self.compile_expr(&if_.else_);
        self.emit(Instr::Mov { dest, src: res });
        self.emit(Instr::Jump(done));

        self.switch_to_block(done);

        dest
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
