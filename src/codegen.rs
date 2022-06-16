use cranelift::{
    codegen::{ir::Function, Context},
    prelude::*,
};

use crate::{compilation::Instr, parsing::BinaryOperator};

pub fn run(instrs: &[Instr]) {
    let func = gen(instrs);

    let mut ctx = Context::for_function(func);

    let builder = isa::lookup_by_name("x86_64-linux").unwrap();
    let target_isa = builder
        .finish(settings::Flags::new(settings::builder()))
        .unwrap();

    let mut output = vec![];
    ctx.compile_and_emit(&*target_isa, &mut output).unwrap();

    let m = mmap::MemoryMap::new(
        output.len(),
        &[mmap::MapOption::MapWritable, mmap::MapOption::MapExecutable],
    )
    .unwrap();
    let d = unsafe { std::slice::from_raw_parts_mut(m.data(), m.len()) };

    for (d, b) in d.iter_mut().zip(output) {
        *d = b;
    }

    let ptr = d.as_ptr();

    type F = fn() -> i64;
    let f = unsafe { std::mem::transmute::<_, F>(ptr) };

    let res = f();

    println!("{}", res);
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

    let mut return_variable = None;
    for instr in instrs {
        match instr {
            &Instr::Const { dest, val } => {
                let var = Variable::with_u32(dest);
                builder.declare_var(var, types::I64);

                let value = builder.ins().iconst(types::I64, val);
                builder.def_var(var, value);
            }
            &Instr::BinOp {
                dest,
                lhs,
                rhs,
                operator,
            } => {
                let var = Variable::with_u32(dest);
                builder.declare_var(var, types::I64);

                let lhs = builder.use_var(Variable::with_u32(lhs));
                let rhs = builder.use_var(Variable::with_u32(rhs));

                let res = gen_bin_op(&mut builder, operator, lhs, rhs);

                builder.def_var(var, res);

                return_variable = Some(var);
            }
        }
    }

    let return_value = builder.use_var(return_variable.unwrap());

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
    lhs: Value,
    rhs: Value,
) -> Value {
    match bin_op {
        BinaryOperator::Plus => builder.ins().iadd(lhs, rhs),
        BinaryOperator::Times => builder.ins().imul(lhs, rhs),
    }
}
