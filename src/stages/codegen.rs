use std::{collections::HashSet, mem, path::Path};

mod cl {
    pub use cranelift::{
        codegen::{
            ir::{
                condcodes::IntCC, types::I64, AbiParam, Block, ExternalName, Function, Signature,
                Value,
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
    bytecode::{Block, Function, Instr},
    common::{BinaryOperator, UnaryOperator},
};

pub fn run_jit(bytecode_func: &Function) -> i64 {
    let codegen_context = CodegenContext::new(bytecode_func);
    let func = codegen_context.gen_func();

    let mut jit_mod =
        cl::JITModule::new(cl::JITBuilder::new(cranelift_module::default_libcall_names()).unwrap());

    let func_id = jit_mod
        .declare_function("_start", cl::Linkage::Export, &func.signature)
        .unwrap();

    let mut ctx = cl::Context::for_function(func);

    jit_mod.define_function(func_id, &mut ctx).unwrap();

    jit_mod.finalize_definitions();

    let func_ptr = jit_mod.get_finalized_function(func_id);

    // SAFETY: who cares about stuff like that?
    let callable: fn() -> i64 = unsafe { mem::transmute(func_ptr) };

    let result = callable();

    // SAFETY: callable and func_ptr are no longer used
    unsafe { jit_mod.free_memory() };

    result
}

pub fn output_to_file<P: AsRef<Path>>(bytecode_func: &Function, filename: P) {
    let target_isa = cl::isa::lookup_by_name("x86_64-linux")
        .unwrap()
        .finish(cl::settings::Flags::new(cl::settings::builder()))
        .unwrap();

    let codegen_context = CodegenContext::new(bytecode_func);
    let func = codegen_context.gen_func();

    let mut obj_mod = cl::ObjectModule::new(
        cl::ObjectBuilder::new(
            target_isa,
            "bingbong",
            cranelift_module::default_libcall_names(),
        )
        .unwrap(),
    );

    let func_id = obj_mod
        .declare_function("_start", cl::Linkage::Export, &func.signature)
        .unwrap();

    let mut ctx = cl::Context::for_function(func);

    obj_mod.define_function(func_id, &mut ctx).unwrap();

    let product = obj_mod.finish();

    let bytes = product.emit().unwrap();

    std::fs::write(filename, bytes).unwrap();
}

struct CodegenContext<'f> {
    blocks: Vec<cl::Block>,
    func: &'f Function,
}

impl<'f> CodegenContext<'f> {
    pub fn new(func: &'f Function) -> Self {
        Self {
            blocks: Vec::new(),
            func,
        }
    }

    pub fn gen_func(mut self) -> cl::Function {
        let sig = cl::Signature {
            params: vec![],
            returns: vec![cl::AbiParam::new(cl::I64)],
            call_conv: cl::isa::CallConv::Fast, // wroom wroom
        };
        let mut func = cl::Function::with_name_signature(cl::ExternalName::user(0, 0), sig);
        let mut fb_ctx = cl::FunctionBuilderContext::new();

        let mut builder = cl::FunctionBuilder::new(&mut func, &mut fb_ctx);

        self.declare_all_vars(&mut builder);

        for _ in &self.func.blocks {
            self.blocks.push(builder.create_block());
        }

        for (i, block) in self.func.blocks.iter().enumerate() {
            builder.switch_to_block(self.blocks[i]);
            self.gen_block(&mut builder, block);
        }

        builder.seal_all_blocks();

        builder.finalize();

        let flags = cl::settings::Flags::new(cl::settings::builder());
        if let Err(err) = cranelift::codegen::verify_function(&func, &flags) {
            panic!("codegen verify: {}", err);
        }

        func
    }

    fn declare_all_vars(&mut self, b: &mut cl::FunctionBuilder) {
        let mut vars = HashSet::new();
        for block in &self.func.blocks {
            for instr in &block.instrs {
                let var = match *instr {
                    Instr::Const { dest, .. } => dest,
                    Instr::BinaryOperator { dest, .. } => dest,
                    Instr::UnaryOperator { dest, .. } => dest,
                    Instr::Mov { dest, .. } => dest,
                    Instr::Jump(_) => continue,
                    Instr::JumpIf { .. } => continue,
                    Instr::Return(val) => val,
                };
                vars.insert(var);
            }
        }
        for var in vars {
            b.declare_var(cl::Variable::with_u32(var), cl::I64);
        }
    }

    fn gen_block(&mut self, b: &mut cl::FunctionBuilder, block: &Block) {
        for instr in &block.instrs {
            self.gen_instr(b, instr)
        }
    }

    fn gen_instr(&mut self, b: &mut cl::FunctionBuilder, instr: &Instr) {
        match *instr {
            Instr::Const { dest, val } => {
                let res = b.ins().iconst(cl::I64, val);
                b.def_var(cl::Variable::with_u32(dest), res);
            }
            Instr::BinaryOperator {
                dest,
                lhs,
                rhs,
                operator,
            } => {
                let lhs = b.use_var(cl::Variable::with_u32(lhs));
                let rhs = b.use_var(cl::Variable::with_u32(rhs));
                let res = self.gen_bin_op(b, operator, lhs, rhs);
                b.def_var(cl::Variable::with_u32(dest), res);
            }
            Instr::UnaryOperator {
                dest,
                operand,
                operator,
            } => {
                let operand = b.use_var(cl::Variable::with_u32(operand));
                let res = self.gen_unary_op(b, operator, operand);
                b.def_var(cl::Variable::with_u32(dest), res);
            }
            Instr::Mov { dest, src } => {
                let val = b.use_var(cl::Variable::with_u32(src));
                b.def_var(cl::Variable::with_u32(dest), val);
            }
            Instr::Jump(block_id) => {
                let dest = self.blocks[block_id as usize];
                b.ins().jump(dest, &[]);
            }
            Instr::JumpIf { cond, block_id } => {
                let cond = b.use_var(cl::Variable::with_u32(cond));
                let dest = self.blocks[block_id as usize];
                b.ins().brnz(cond, dest, &[]);
            }
            Instr::Return(val) => {
                let val = b.use_var(cl::Variable::with_u32(val));
                b.ins().return_(&[val]);
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
}
