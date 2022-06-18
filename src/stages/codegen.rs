use std::{mem, ops};

use cranelift::{
    codegen::{
        ir::{types, AbiParam, ExternalName, Function, InstBuilder, Signature, Value as CLValue},
        isa, settings, Context,
    },
    frontend::{FunctionBuilder, FunctionBuilderContext},
};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};

use crate::{
    bytecode::{Instr, Value},
    common::BinaryOperator,
};

pub fn run_jit(instrs: &[Instr]) -> i64 {
    let func = gen(instrs);

    let mut jit_mod =
        JITModule::new(JITBuilder::new(cranelift_module::default_libcall_names()).unwrap());

    let func_id = jit_mod
        .declare_function("_start", Linkage::Export, &func.signature)
        .unwrap();

    let mut ctx = Context::for_function(func);

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

pub fn output_to_file(instrs: &[Instr]) {
    let target_isa = isa::lookup_by_name("x86_64-linux")
        .unwrap()
        .finish(settings::Flags::new(settings::builder()))
        .unwrap();

    let func = gen(instrs);

    let mut obj_mod = ObjectModule::new(
        ObjectBuilder::new(
            target_isa,
            "bingbong",
            cranelift_module::default_libcall_names(),
        )
        .unwrap(),
    );

    let func_id = obj_mod
        .declare_function("_start", Linkage::Export, &func.signature)
        .unwrap();

    let mut ctx = Context::for_function(func);

    obj_mod.define_function(func_id, &mut ctx).unwrap();

    let product = obj_mod.finish();

    let bytes = product.emit().unwrap();

    std::fs::write("output.o", bytes).unwrap();
}

fn gen(instrs: &[Instr]) -> Function {
    let sig = Signature {
        params: vec![],
        returns: vec![AbiParam::new(types::I64)],
        call_conv: isa::CallConv::Fast, // wroom wroom
    };
    let mut func = Function::with_name_signature(ExternalName::user(0, 0), sig);
    let mut fb_ctx = FunctionBuilderContext::new();

    let mut builder = FunctionBuilder::new(&mut func, &mut fb_ctx);
    let block = builder.create_block();
    builder.switch_to_block(block);
    builder.seal_block(block);

    let mut values = ValueConverter::empty();

    for instr in instrs {
        match *instr {
            Instr::Const { dest, val } => {
                let res = builder.ins().iconst(types::I64, val);
                values[dest] = res;
            }
            Instr::BinOp {
                dest,
                lhs,
                rhs,
                operator,
            } => {
                let lhs = values[lhs];
                let rhs = values[rhs];

                let res = gen_bin_op(&mut builder, operator, lhs, rhs);

                values[dest] = res;
            }
        }
    }

    let return_value = values.last().unwrap();

    builder.ins().return_(&[return_value]);

    builder.finalize();

    let flags = settings::Flags::new(settings::builder());
    if let Err(err) = cranelift::codegen::verify_function(&func, &flags) {
        panic!("codegen: {}", err);
    }

    func
}

fn gen_bin_op(
    builder: &mut FunctionBuilder,
    bin_op: BinaryOperator,
    lhs: CLValue,
    rhs: CLValue,
) -> CLValue {
    match bin_op {
        BinaryOperator::Add => builder.ins().iadd(lhs, rhs),
        BinaryOperator::Subtract => builder.ins().isub(lhs, rhs),
        BinaryOperator::Multiply => builder.ins().imul(lhs, rhs),
        BinaryOperator::Divide => builder.ins().sdiv(lhs, rhs),
    }
}

struct ValueConverter {
    values: Vec<CLValue>,
}

impl ValueConverter {
    pub fn empty() -> Self {
        Self { values: Vec::new() }
    }
    pub fn last(&self) -> Option<CLValue> {
        self.values.last().copied()
    }
}

impl ops::Index<Value> for ValueConverter {
    type Output = CLValue;

    fn index(&self, index: Value) -> &Self::Output {
        &self.values[index as usize]
    }
}

impl ops::IndexMut<Value> for ValueConverter {
    fn index_mut(&mut self, index: Value) -> &mut Self::Output {
        for _ in self.values.len()..=index as usize {
            self.values.push(CLValue::from_u32(0));
        }
        &mut self.values[index as usize]
    }
}
