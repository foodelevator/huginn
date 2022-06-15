use std::assert_matches::assert_matches;

use crate::{
    compilation::{Compilation, Instr},
    lexing::{Lexer, TokenKind},
    parsing::{BinaryOperation, BinaryOperator, Expr},
};

#[test]
fn lex() {
    let input = "
    )23 1 + 2+81
    (
    ";

    let tokens: Vec<_> = Lexer::new(input.chars().peekable())
        .map(|token| token.kind)
        .collect();

    assert_eq!(
        tokens,
        vec![
            TokenKind::RightParen,
            TokenKind::Int(23),
            TokenKind::Int(1),
            TokenKind::Plus,
            TokenKind::Int(2),
            TokenKind::Plus,
            TokenKind::Int(81),
            TokenKind::LeftParen,
        ]
    );
}

#[test]
fn parse_basic_arithmetic() {
    let mut lexer = Lexer::new("1 + 2 * 3".chars().peekable()).peekable();
    assert_matches!(
        Expr::parse(&mut lexer),
        Expr::BinaryOperation(box BinaryOperation {
            lhs: Expr::Int(_, 1),
            operator: BinaryOperator::Plus,
            rhs: Expr::BinaryOperation(box BinaryOperation {
                lhs: Expr::Int(_, 2),
                operator: BinaryOperator::Times,
                rhs: Expr::Int(_, 3),
                ..
            }),
            ..
        })
    );
    assert_eq!(lexer.next(), None);

    let mut lexer = Lexer::new("1 * 2 + 3".chars().peekable()).peekable();
    assert_matches!(
        Expr::parse(&mut lexer),
        Expr::BinaryOperation(box BinaryOperation {
            lhs: Expr::BinaryOperation(box BinaryOperation {
                lhs: Expr::Int(_, 1),
                operator: BinaryOperator::Times,
                rhs: Expr::Int(_, 2),
                ..
            }),
            operator: BinaryOperator::Plus,
            rhs: Expr::Int(_, 3),
            ..
        })
    );
    assert_eq!(lexer.next(), None);

    let mut lexer = Lexer::new("(1 + 2) * 3".chars().peekable()).peekable();
    assert_matches!(
        Expr::parse(&mut lexer),
        Expr::BinaryOperation(box BinaryOperation {
            lhs: Expr::Grouping {
                expr: box Expr::BinaryOperation(box BinaryOperation {
                    lhs: Expr::Int(_, 1),
                    operator: BinaryOperator::Plus,
                    rhs: Expr::Int(_, 2),
                    ..
                }),
                ..
            },
            operator: BinaryOperator::Times,
            rhs: Expr::Int(_, 3),
            ..
        })
    );
    assert_eq!(lexer.next(), None);

    let mut lexer = Lexer::new("1 * (2 + 3)".chars().peekable()).peekable();
    assert_matches!(
        Expr::parse(&mut lexer),
        Expr::BinaryOperation(box BinaryOperation {
            lhs: Expr::Int(_, 1),
            operator: BinaryOperator::Times,
            rhs: Expr::Grouping {
                expr: box Expr::BinaryOperation(box BinaryOperation {
                    lhs: Expr::Int(_, 2),
                    operator: BinaryOperator::Plus,
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
    let mut lexer = Lexer::new("1 + 2 * 3".chars().peekable()).peekable();
    let expr = Expr::parse(&mut lexer);
    assert_matches!(
        &Compilation::compile(&expr)[..],
        &[
            Instr::Const { val: 1, dest: one_a },
            Instr::Const { val: 2, dest: two_a },
            Instr::Const { val: 3, dest: three_a },
            Instr::BinOp {
                operator: BinaryOperator::Times,
                lhs: two_b,
                rhs: three_b,
                dest: prod_a,
                ..
            },
            Instr::BinOp {
                operator: BinaryOperator::Plus,
                lhs: one_b,
                rhs: prod_b,
                ..
            },
        ] if one_a == one_b && two_a == two_b && three_a == three_b && prod_a == prod_b
    );

    let mut lexer = Lexer::new("1 * 2 + 3".chars().peekable()).peekable();
    let expr = Expr::parse(&mut lexer);
    assert_matches!(
        &Compilation::compile(&expr)[..],
        &[
            Instr::Const { val: 1, dest: one_a },
            Instr::Const { val: 2, dest: two_a },
            Instr::BinOp {
                operator: BinaryOperator::Times,
                lhs: one_b,
                rhs: two_b,
                dest: prod_a,
                ..
            },
            Instr::Const { val: 3, dest: three_a },
            Instr::BinOp {
                operator: BinaryOperator::Plus,
                lhs: prod_b,
                rhs: three_b,
                ..
            },
        ] if one_a == one_b && two_a == two_b && three_a == three_b && prod_a == prod_b
    );
}
