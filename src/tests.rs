use std::assert_matches::assert_matches;

use crate::{
    bytecode::Instr,
    codegen,
    common::BinaryOperator,
    compilation::Compilation,
    lexing::Lexer,
    parsing::parse_expr,
    syntax_tree::{BinaryOperation, Expr},
    tokens::TokenKind,
};

#[test]
fn lex() {
    let input = "
    )-23 1 + 2+81/3*2
    (
    ";

    let tokens: Vec<_> = Lexer::new(input.chars().peekable())
        .map(|token| token.kind)
        .collect();

    assert_eq!(
        tokens,
        vec![
            TokenKind::RightParen,
            TokenKind::Minus,
            TokenKind::Int(23),
            TokenKind::Int(1),
            TokenKind::Plus,
            TokenKind::Int(2),
            TokenKind::Plus,
            TokenKind::Int(81),
            TokenKind::Slash,
            TokenKind::Int(3),
            TokenKind::Asterix,
            TokenKind::Int(2),
            TokenKind::LeftParen,
        ]
    );
}

#[test]
fn parse_basic_arithmetic() {
    let mut lexer = Lexer::new("1 + 2 * 3 - 4 / 5".chars().peekable()).peekable();
    assert_matches!(
        parse_expr(&mut lexer),
        Expr::BinaryOperation(box BinaryOperation {
            lhs: Expr::BinaryOperation(box BinaryOperation {
                lhs: Expr::Int(_, 1),
                operator: BinaryOperator::Add,
                rhs: Expr::BinaryOperation(box BinaryOperation {
                    lhs: Expr::Int(_, 2),
                    operator: BinaryOperator::Multiply,
                    rhs: Expr::Int(_, 3),
                    ..
                }),
                ..
            }),
            operator: BinaryOperator::Subtract,
            rhs: Expr::BinaryOperation(box BinaryOperation {
                lhs: Expr::Int(_, 4),
                operator: BinaryOperator::Divide,
                rhs: Expr::Int(_, 5),
                ..
            }),
            ..
        }),
    );
    assert_eq!(lexer.next(), None);

    let mut lexer = Lexer::new("1 * 2 + 3".chars().peekable()).peekable();
    assert_matches!(
        parse_expr(&mut lexer),
        Expr::BinaryOperation(box BinaryOperation {
            lhs: Expr::BinaryOperation(box BinaryOperation {
                lhs: Expr::Int(_, 1),
                operator: BinaryOperator::Multiply,
                rhs: Expr::Int(_, 2),
                ..
            }),
            operator: BinaryOperator::Add,
            rhs: Expr::Int(_, 3),
            ..
        })
    );
    assert_eq!(lexer.next(), None);

    let mut lexer = Lexer::new("(1 + 2) * 3".chars().peekable()).peekable();
    assert_matches!(
        parse_expr(&mut lexer),
        Expr::BinaryOperation(box BinaryOperation {
            lhs: Expr::Grouping {
                expr: box Expr::BinaryOperation(box BinaryOperation {
                    lhs: Expr::Int(_, 1),
                    operator: BinaryOperator::Add,
                    rhs: Expr::Int(_, 2),
                    ..
                }),
                ..
            },
            operator: BinaryOperator::Multiply,
            rhs: Expr::Int(_, 3),
            ..
        })
    );
    assert_eq!(lexer.next(), None);

    let mut lexer = Lexer::new("1 * (2 + 3)".chars().peekable()).peekable();
    assert_matches!(
        parse_expr(&mut lexer),
        Expr::BinaryOperation(box BinaryOperation {
            lhs: Expr::Int(_, 1),
            operator: BinaryOperator::Multiply,
            rhs: Expr::Grouping {
                expr: box Expr::BinaryOperation(box BinaryOperation {
                    lhs: Expr::Int(_, 2),
                    operator: BinaryOperator::Add,
                    rhs: Expr::Int(_, 3),
                    ..
                }),
                ..
            },
            ..
        })
    );
    assert_eq!(lexer.next(), None);
}

#[test]
fn compile_basic_arithmetic() {
    let mut lexer = Lexer::new("1 + 2 * 3 - 4 / 5".chars().peekable()).peekable();
    let expr = parse_expr(&mut lexer);
    assert_matches!(
        &Compilation::compile(&expr)[..],
        &[
            Instr::Const { val: 1, dest: one_a },
            Instr::Const { val: 2, dest: two_a },
            Instr::Const { val: 3, dest: three_a },
            Instr::BinOp {
                operator: BinaryOperator::Multiply,
                lhs: two_b,
                rhs: three_b,
                dest: prod_1_a,
                ..
            },
            Instr::BinOp {
                operator: BinaryOperator::Add,
                lhs: one_b,
                rhs: prod_1_b,
                dest: prod_2_a,
                ..
            },
            Instr::Const { val: 4, dest: four_a },
            Instr::Const { val: 5, dest: five_a },
            Instr::BinOp {
                operator: BinaryOperator::Divide,
                lhs: four_b,
                rhs: five_b,
                dest: prod_3_a,
                ..
            },
            Instr::BinOp {
                operator: BinaryOperator::Subtract,
                lhs: prod_2_b,
                rhs: prod_3_b,
                ..
            },
        ] if one_a == one_b &&
             two_a == two_b &&
             three_a == three_b &&
             four_a == four_b &&
             five_a == five_b &&
             prod_1_a == prod_1_b &&
             prod_2_a == prod_2_b &&
             prod_3_a == prod_3_b
    );
}

#[test]
fn codegen_basic_arithmetic() {
    let mut lexer = Lexer::new("1 + 2 * 3 - 4 / 5".chars().peekable()).peekable();
    let expr = parse_expr(&mut lexer);
    let instrs = Compilation::compile(&expr);
    let res = codegen::run_jit(&instrs);
    assert_eq!(1 + 2 * 3 - 4 / 5, res);
}
