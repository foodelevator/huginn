use std::assert_matches::assert_matches;

use crate::{
    bytecode::Instr, common::UnaryOperator, lexing::Lexer, lowering::lower_file, parsing::Parser,
};

mod basic_stuff;

#[test]
fn print() {
    let src = include_str!("print.hg");

    let (mut d1, mut d2) = (vec![], vec![]);
    let mut lexer = Lexer::new(src.chars().peekable(), 0, &mut d1).peekable();
    let mut parser = Parser::new(&mut lexer, &mut d2);
    let file = parser.file().unwrap();
    let func = lower_file(&file);

    assert!(d1.is_empty(), "{:?}", d1);
    assert!(d2.is_empty(), "{:?}", d2);
    assert_eq!(func.blocks.len(), 1);
    assert_matches!(
        &func.blocks[0].instrs[..],
        &[
            Instr::Const { dest: ten_a_1, val: 10 },
            Instr::Print(ten_a_2),

            Instr::Const { dest: two_a_1, val: 2 },
            Instr::Print(two_a_2),

            Instr::Const { dest: one_a_1, val: 1 },
            Instr::Print(one_a_2),

            Instr::Const { dest: zero_1, val: 0 },
            Instr::Print(zero_2),

            Instr::Const { dest: one_b_1, val: 1 },
            Instr::UnaryOperation { dest: n_one_1, operand: one_b_2, operator: UnaryOperator::Negate },
            Instr::Print(n_one_2),

            Instr::Const { dest: two_b_1, val: 2 },
            Instr::UnaryOperation { dest: n_two_1, operand: two_b_2, operator: UnaryOperator::Negate },
            Instr::Print(n_two_2),

            Instr::Const { dest: ten_b_1, val: 10 },
            Instr::UnaryOperation { dest: n_ten_1, operand: ten_b_2, operator: UnaryOperator::Negate },
            Instr::Print(n_ten_2),

            Instr::Const { dest: ret_1, val: 0 },
            Instr::Return(ret_2),

            // Instr::Const { dest: ten_a_1, val: 10 },
            // Instr::UnaryOperation { dest: n_ten_1, operand: ten_a_2, operator: UnaryOperator::Negate },
            // Instr::Print(n_ten_2),
        ] if ten_a_1 == ten_a_2
            && two_a_1 == two_a_2
            && one_a_1 == one_a_2
            && zero_1 == zero_2
            && one_b_1 == one_b_2
            && n_one_1 == n_one_2
            && two_b_1 == two_b_2
            && n_two_1 == n_two_2
            && ten_b_1 == ten_b_2
            && n_ten_1 == n_ten_2
            && ret_1 == ret_2,
        "{:#?}",
        &func.blocks[0].instrs[..],
    );
}
