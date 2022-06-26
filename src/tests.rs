use std::assert_matches::assert_matches;

use crate::{
    bytecode::Instr,
    codegen,
    common::BinaryOperator,
    compilation::compile,
    lexing::Lexer,
    parsing::Parser,
    syntax_tree::{BinaryOperation, Expr, Grouping},
    tokens::TokenKind,
};

#[test]
fn lex() {
    let input = "else
    )-23 1 + 2!=+81/3*2 if=>=
    (
    ";

    let mut d = vec![];
    let tokens: Vec<_> = Lexer::new(input.chars().peekable(), &mut d)
        .map(|token| token.kind)
        .collect();

    assert!(d.is_empty(), "{:?}", d);
    assert_eq!(
        tokens,
        vec![
            TokenKind::Else,
            TokenKind::RightParen,
            TokenKind::Minus,
            TokenKind::Int(23),
            TokenKind::Int(1),
            TokenKind::Plus,
            TokenKind::Int(2),
            TokenKind::BangEqual,
            TokenKind::Plus,
            TokenKind::Int(81),
            TokenKind::Slash,
            TokenKind::Int(3),
            TokenKind::Asterix,
            TokenKind::Int(2),
            TokenKind::If,
            TokenKind::Equal,
            TokenKind::GreaterEqual,
            TokenKind::LeftParen,
        ]
    );
}

#[test]
fn parse_basic_arithmetic() {
    let (mut d1, mut d2) = (vec![], vec![]);
    let mut lexer = Lexer::new("1 + 2 * 3 - 4 / 5".chars().peekable(), &mut d1).peekable();
    let expr = Parser::new(&mut lexer, &mut d2).expr();
    assert_eq!(lexer.next(), None);
    assert!(d1.is_empty(), "{:?}", d1);
    assert!(d2.is_empty(), "{:?}", d2);
    assert_matches!(
        expr,
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

    let (mut d1, mut d2) = (vec![], vec![]);
    let mut lexer = Lexer::new("1 * 2 + 3".chars().peekable(), &mut d1).peekable();
    let expr = Parser::new(&mut lexer, &mut d2).expr();
    assert_eq!(lexer.next(), None);
    assert!(d1.is_empty(), "{:?}", d1);
    assert!(d2.is_empty(), "{:?}", d2);
    assert_matches!(
        expr,
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

    let (mut d1, mut d2) = (vec![], vec![]);
    let mut lexer = Lexer::new("(1 + 2) * 3".chars().peekable(), &mut d1).peekable();
    let expr = Parser::new(&mut lexer, &mut d2).expr();
    assert_eq!(lexer.next(), None);
    assert!(d1.is_empty(), "{:?}", d1);
    assert!(d2.is_empty(), "{:?}", d2);
    assert_matches!(
        expr,
        Expr::BinaryOperation(box BinaryOperation {
            lhs: Expr::Grouping(Grouping {
                expr: box Expr::BinaryOperation(box BinaryOperation {
                    lhs: Expr::Int(_, 1),
                    operator: BinaryOperator::Add,
                    rhs: Expr::Int(_, 2),
                    ..
                }),
                ..
            }),
            operator: BinaryOperator::Multiply,
            rhs: Expr::Int(_, 3),
            ..
        })
    );

    let (mut d1, mut d2) = (vec![], vec![]);
    let mut lexer = Lexer::new("1 * (2 + 3)".chars().peekable(), &mut d1).peekable();
    let expr = Parser::new(&mut lexer, &mut d2).expr();
    assert_eq!(lexer.next(), None);
    assert!(d1.is_empty(), "{:?}", d1);
    assert!(d2.is_empty(), "{:?}", d2);
    assert_matches!(
        expr,
        Expr::BinaryOperation(box BinaryOperation {
            lhs: Expr::Int(_, 1),
            operator: BinaryOperator::Multiply,
            rhs: Expr::Grouping(Grouping {
                expr: box Expr::BinaryOperation(box BinaryOperation {
                    lhs: Expr::Int(_, 2),
                    operator: BinaryOperator::Add,
                    rhs: Expr::Int(_, 3),
                    ..
                }),
                ..
            }),
            ..
        })
    );
}

#[test]
fn compile_basic_arithmetic() {
    let (mut d1, mut d2) = (vec![], vec![]);
    let mut lexer = Lexer::new("1 + 2 * 3 - 4 / 5".chars().peekable(), &mut d1).peekable();
    let mut parser = Parser::new(&mut lexer, &mut d2);
    let stmt = parser.stmt();
    let func = compile(&[stmt]);
    assert!(d1.is_empty(), "{:?}", d1);
    assert!(d2.is_empty(), "{:?}", d2);
    assert_eq!(func.blocks.len(), 1);
    assert_matches!(
        &func.blocks[0].instrs[..],
        &[
            Instr::Const { val: 1, dest: one_a },
            Instr::Const { val: 2, dest: two_a },
            Instr::Const { val: 3, dest: three_a },
            Instr::BinaryOperator {
                operator: BinaryOperator::Multiply,
                lhs: two_b,
                rhs: three_b,
                dest: prod_1_a,
                ..
            },
            Instr::BinaryOperator {
                operator: BinaryOperator::Add,
                lhs: one_b,
                rhs: prod_1_b,
                dest: prod_2_a,
                ..
            },
            Instr::Const { val: 4, dest: four_a },
            Instr::Const { val: 5, dest: five_a },
            Instr::BinaryOperator {
                operator: BinaryOperator::Divide,
                lhs: four_b,
                rhs: five_b,
                dest: prod_3_a,
                ..
            },
            Instr::BinaryOperator {
                operator: BinaryOperator::Subtract,
                lhs: prod_2_b,
                rhs: prod_3_b,
                dest: prod_4_a,
                ..
            },
            Instr::Return(prod_4_b),
        ] if one_a == one_b &&
             two_a == two_b &&
             three_a == three_b &&
             four_a == four_b &&
             five_a == five_b &&
             prod_1_a == prod_1_b &&
             prod_2_a == prod_2_b &&
             prod_3_a == prod_3_b &&
             prod_4_a == prod_4_b
    );
}

fn test_code(code: &'static str, expected: i64) {
    let (mut d1, mut d2) = (vec![], vec![]);
    let mut lexer = Lexer::new(code.chars().peekable(), &mut d1).peekable();
    let mut parser = Parser::new(&mut lexer, &mut d2);
    let stmt = parser.stmt();
    assert!(d1.is_empty(), "{:?}", d1);
    assert!(d2.is_empty(), "{:?}", d2);
    let func = compile(&[stmt]);
    let res = codegen::run_jit(&func);
    assert_eq!(expected, res);
}

#[test]
fn codegen_basic_arithmetic() {
    test_code("1 + 2 * 3 - 4 / 5", 1 + 2 * 3 - 4 / 5);
}

#[test]
fn codegen_booleans() {
    for (code, expected) in [("1 <= 2", 1), ("!-2", 0), ("3 = 1 + 2", 1)] {
        test_code(code, expected);
    }
}

#[test]
fn codegen_if() {
    test_code("if 69 { 1337 } else { 42 } + 42", 1337 + 42);
}
