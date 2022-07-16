use std::assert_matches::assert_matches;

use crate::{
    bytecode::Instr, common::UnaryOperator, lexing::Lexer, lowering::lower_file, parsing::Parser,
};

mod basic_stuff;

#[test]
fn print() {
    let src = include_str!("print.hg");

    let mut lexer = Lexer::new(src.chars().peekable(), 0);
    let mut parser = Parser::new(&mut lexer);
    let file = parser.file().unwrap();
    let module = lower_file(&file);

    assert!(
        parser.diagnostics().is_empty(),
        "{:?}",
        parser.diagnostics()
    );
    assert!(lexer.diagnostics().is_empty(), "{:?}", lexer.diagnostics());

    assert_eq!(module.scope.len(), 1);
    assert_eq!(module.symbols.len(), 1);

    let proc = &module.symbols[module.scope["main"]];

    assert_eq!(proc.blocks.len(), 1);
    assert_matches!(
        &proc.blocks[0].instrs[..],
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
        &proc.blocks[0].instrs[..],
    );
}
