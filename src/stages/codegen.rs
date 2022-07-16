use std::{collections::HashMap, mem};

mod cl {
    pub use cranelift::{
        codegen::{
            ir::{
                condcodes::IntCC, types::I64, AbiParam, Block, ExtFuncData, ExternalName, FuncRef,
                Function, SigRef, Signature, TrapCode, Value,
            },
            isa, settings, Context,
        },
        frontend::{FunctionBuilder, FunctionBuilderContext, Variable},
    };
    pub use cranelift_jit::{JITBuilder, JITModule};
    pub use cranelift_module::Linkage;
    pub use cranelift_object::{ObjectBuilder, ObjectModule};
}
use cranelift::codegen::ir::InstBuilder as _;
use cranelift_module::Module as _;

use crate::{
    bitcode::{Block, BlockId, Instr, Module, Procedure, Value},
    common::{BinaryOperator, UnaryOperator},
    Array,
};

fn sig() -> cl::Signature {
    cl::Signature {
        params: vec![cl::AbiParam::new(cl::I64)],
        returns: vec![cl::AbiParam::new(cl::I64)],
        call_conv: cl::isa::CallConv::SystemV,
    }
}

pub fn run_jit(bitcode_func: &Procedure) -> i64 {
    let mut builder = cl::JITBuilder::new(cranelift_module::default_libcall_names()).unwrap();

    builder.symbol("print", super::jit::print as *const u8);

    let mut jit_mod = cl::JITModule::new(builder);

    let mut main_func = cl::Function::with_name_signature(cl::ExternalName::user(0, 0), sig());

    let print_sig_ref = main_func.import_signature(sig());

    let main_id = jit_mod
        .declare_function("main", cl::Linkage::Export, &main_func.signature)
        .unwrap();
    let print_id = jit_mod
        .declare_function("print", cl::Linkage::Import, &sig())
        .unwrap();

    let print_ref = jit_mod.declare_func_in_func(print_id, &mut main_func);

    let codegen_context = CodegenContext::new(bitcode_func, print_ref, print_sig_ref);
    codegen_context.gen_func(&mut main_func);

    let mut ctx = cl::Context::for_function(main_func);
    jit_mod.define_function(main_id, &mut ctx).unwrap();

    jit_mod.finalize_definitions();

    let func_ptr = jit_mod.get_finalized_function(main_id);

    // SAFETY: The calling convention & signature are correct,
    // and so far the language can't do anything unsafe :^)
    let callable: extern "C" fn() -> i64 = unsafe { mem::transmute(func_ptr) };

    let result = callable();

    // SAFETY: callable and func_ptr are no longer used
    unsafe { jit_mod.free_memory() };

    result
}

pub fn build_object(bitcode_mod: &Module) -> Vec<u8> {
    let target_isa = cl::isa::lookup_by_name("x86_64-linux")
        .unwrap()
        .finish(cl::settings::Flags::new(cl::settings::builder()))
        .unwrap();

    let mut obj_mod = cl::ObjectModule::new(
        cl::ObjectBuilder::new(
            target_isa,
            "bingbong", // lol
            cranelift_module::default_libcall_names(),
        )
        .unwrap(),
    );

    let print_id = obj_mod
        .declare_function("print", cl::Linkage::Import, &sig())
        .unwrap();

    let mut ctx = cl::Context::new();
    for (id, proc) in bitcode_mod.procedures.enumerate() {
        let mut func = cl::Function::with_name_signature(cl::ExternalName::user(0, id), sig());

        let func_id = obj_mod
            .declare_function(&proc.name, cl::Linkage::Export, &func.signature)
            .unwrap();

        let print_ref = obj_mod.declare_func_in_func(print_id, &mut func);
        let print_sig_ref = func.import_signature(sig());

        let codegen_ctx = CodegenContext::new(proc, print_ref, print_sig_ref);
        codegen_ctx.gen_func(&mut func);

        ctx.clear();
        ctx.func = func;
        obj_mod.define_function(func_id, &mut ctx).unwrap();
    }

    let product = obj_mod.finish();

    product.emit().unwrap()
}

struct CodegenContext<'f> {
    blocks: Array<BlockId, cl::Block>,
    proc: &'f Procedure,
    values: HashMap<u32, cl::Value>,
    sig_ref: cl::SigRef,
    print_func: cl::FuncRef,
}

impl<'f> CodegenContext<'f> {
    pub fn new(proc: &'f Procedure, print_func: cl::FuncRef, sig_ref: cl::SigRef) -> Self {
        Self {
            blocks: Array::default(),
            proc,
            values: HashMap::new(),
            sig_ref,
            print_func,
        }
    }

    pub fn gen_func(mut self, func: &mut cl::Function) {
        let mut fb_ctx = cl::FunctionBuilderContext::new();
        let mut b = cl::FunctionBuilder::new(func, &mut fb_ctx);

        self.declare_vars(&mut b);

        for _ in self.proc.blocks.values() {
            self.blocks.push(b.create_block());
        }

        for (i, block) in self.proc.blocks.enumerate() {
            if i == 0 {
                b.append_block_params_for_function_params(self.blocks[i]);
            }
            b.switch_to_block(self.blocks[i]);
            self.gen_block(&mut b, block);
        }

        b.seal_all_blocks();

        b.finalize();

        let flags = cl::settings::Flags::new(cl::settings::builder());
        if let Err(err) = cranelift::codegen::verify_function(func, &flags) {
            panic!("compiler bug: codegen verify: {}", err);
        }
    }

    fn declare_vars(&mut self, b: &mut cl::FunctionBuilder) {
        for value in self.proc.values.keys() {
            if let Ok(var) = self.get_var(*value) {
                b.declare_var(var, cl::I64);
            }
        }
    }

    fn gen_block(&mut self, b: &mut cl::FunctionBuilder, block: &Block) {
        for instr in &block.instrs {
            self.gen_instr(b, instr);
            if b.is_filled() {
                break;
            }
        }
    }

    fn gen_instr(&mut self, b: &mut cl::FunctionBuilder, instr: &Instr) {
        match *instr {
            Instr::Const { dest, val } => {
                let val = b.ins().iconst(cl::I64, val);
                self.set_value(b, dest, val);
            }
            Instr::BinaryOperation {
                dest,
                lhs,
                rhs,
                operator,
            } => {
                let lhs = self.get_value(b, lhs);
                let rhs = self.get_value(b, rhs);
                let res = self.gen_bin_op(b, operator, lhs, rhs);
                self.set_value(b, dest, res);
            }
            Instr::UnaryOperation {
                dest,
                operand,
                operator,
            } => {
                let operand = self.get_value(b, operand);
                let res = self.gen_unary_op(b, operator, operand);
                self.set_value(b, dest, res);
            }
            Instr::Jump(block_id) => {
                let dest = self.blocks[block_id];
                b.ins().jump(dest, &[]);
            }
            Instr::Branch {
                cond,
                then_block,
                else_block,
            } => {
                let cond = self.get_value(b, cond);
                b.ins().brnz(cond, self.blocks[then_block], &[]);
                b.ins().jump(self.blocks[else_block], &[]);
            }
            Instr::Print(val) => {
                let val = self.get_value(b, val);
                b.ins().call(self.print_func, &[val]);

                // let f = b.ins().func_addr(cl::I64, self.print_func);
                // b.ins().call_indirect(self.sig_ref, f, &[val]);
            }
            Instr::Return(val) => {
                let val = self.get_value(b, val);
                b.ins().return_(&[val]);
            }
            Instr::Mov { dest, src } => {
                let res = self.get_value(b, src);
                self.set_value(b, dest, res);
            }
        }
    }

    fn gen_bin_op(
        &mut self,
        b: &mut cl::FunctionBuilder,
        bin_op: BinaryOperator,
        lhs: cl::Value,
        rhs: cl::Value,
    ) -> cl::Value {
        match bin_op {
            BinaryOperator::Add => b.ins().iadd(lhs, rhs),
            BinaryOperator::Subtract => b.ins().isub(lhs, rhs),
            BinaryOperator::Multiply => b.ins().imul(lhs, rhs),
            BinaryOperator::Divide => b.ins().sdiv(lhs, rhs),
            BinaryOperator::Less => {
                let c = b.ins().icmp(cl::IntCC::SignedLessThan, lhs, rhs);
                b.ins().bint(cl::I64, c)
            }
            BinaryOperator::Greater => {
                let c = b.ins().icmp(cl::IntCC::SignedGreaterThan, lhs, rhs);
                b.ins().bint(cl::I64, c)
            }
            BinaryOperator::Equal => {
                let c = b.ins().icmp(cl::IntCC::Equal, lhs, rhs);
                b.ins().bint(cl::I64, c)
            }
            BinaryOperator::NotEqual => {
                let c = b.ins().icmp(cl::IntCC::NotEqual, lhs, rhs);
                b.ins().bint(cl::I64, c)
            }
            BinaryOperator::LessEqual => {
                let c = b.ins().icmp(cl::IntCC::SignedLessThanOrEqual, lhs, rhs);
                b.ins().bint(cl::I64, c)
            }
            BinaryOperator::GreaterEqual => {
                let c = b.ins().icmp(cl::IntCC::SignedGreaterThanOrEqual, lhs, rhs);
                b.ins().bint(cl::I64, c)
            }
        }
    }

    fn gen_unary_op(
        &mut self,
        b: &mut cl::FunctionBuilder,
        unary_op: UnaryOperator,
        operand: cl::Value,
    ) -> cl::Value {
        match unary_op {
            UnaryOperator::Not => {
                let c = b.ins().icmp_imm(cl::IntCC::Equal, operand, 0);
                b.ins().bint(cl::I64, c)
            }
            UnaryOperator::Negate => b.ins().ineg(operand),
        }
    }

    fn get_value(&self, b: &mut cl::FunctionBuilder, value: Value) -> cl::Value {
        match self.get_var(value) {
            Ok(var) => b.use_var(var),
            Err(value) => self.values[&value],
        }
    }

    fn set_value(&mut self, b: &mut cl::FunctionBuilder, dest: Value, src: cl::Value) {
        match self.get_var(dest) {
            Ok(var) => {
                b.def_var(var, src);
            }
            Err(dest) => {
                self.values.insert(dest, src);
            }
        }
    }

    fn get_var(&self, value: Value) -> Result<cl::Variable, u32> {
        let info = self.proc.values.get(&value).copied().unwrap_or_default();
        match value {
            Value::Local(index) => Ok(cl::Variable::with_u32(index)),
            Value::Temp(index) if info.writes > 1 => {
                Ok(cl::Variable::with_u32(index + self.proc.local_count))
            }
            Value::Temp(index) => Err(index),
        }
    }
}
