use std::assert_matches::assert_matches;

use crate::{
    bytecode::Instr,
    codegen,
    common::{BinaryOperator, Ident},
    lexing::Lexer,
    lowering::{lower_expr, lower_file},
    resolution::resolve,
    parsing::Parser,
    syntax_tree::{BinaryOperation, Expr, Grouping, Proc, Stmt, VarDecl},
    tokens::TokenKind,
};

#[test]
fn lex() {
    let input = "else
    )-23 1 + 2!=+81/3*2 if=>=
    (
    ";

    let mut d = vec![];
    let tokens: Vec<_> = Lexer::new(input.chars().peekable(), 0, &mut d)
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
    let mut lexer = Lexer::new("1 + 2 * 3 - 4 / 5".chars().peekable(), 0, &mut d1).peekable();
    let expr = Parser::new(&mut lexer, &mut d2).expr().unwrap();
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
    let mut lexer = Lexer::new("1 * 2 + 3".chars().peekable(), 0, &mut d1).peekable();
    let expr = Parser::new(&mut lexer, &mut d2).expr().unwrap();
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
    let mut lexer = Lexer::new("(1 + 2) * 3".chars().peekable(), 0, &mut d1).peekable();
    let expr = Parser::new(&mut lexer, &mut d2).expr().unwrap();
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
    let mut lexer = Lexer::new("1 * (2 + 3)".chars().peekable(), 0, &mut d1).peekable();
    let expr = Parser::new(&mut lexer, &mut d2).expr().unwrap();
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
fn lower_basic_arithmetic() {
    let (mut d1, mut d2) = (vec![], vec![]);
    let mut lexer = Lexer::new("1 + 2 * 3 - 4 / 5".chars().peekable(), 0, &mut d1).peekable();
    let mut parser = Parser::new(&mut lexer, &mut d2);
    let expr = parser.expr().unwrap();
    let proc = lower_expr(&expr);
    assert!(d1.is_empty(), "{:?}", d1);
    assert!(d2.is_empty(), "{:?}", d2);
    assert_eq!(proc.blocks.len(), 1);
    assert_matches!(
        &proc.blocks[0].instrs[..],
        &[
            Instr::Const { val: 1, dest: one_a },
            Instr::Const { val: 2, dest: two_a },
            Instr::Const { val: 3, dest: three_a },
            Instr::BinaryOperation {
                operator: BinaryOperator::Multiply,
                lhs: two_b,
                rhs: three_b,
                dest: prod_1_a,
                ..
            },
            Instr::BinaryOperation {
                operator: BinaryOperator::Add,
                lhs: one_b,
                rhs: prod_1_b,
                dest: prod_2_a,
                ..
            },
            Instr::Const { val: 4, dest: four_a },
            Instr::Const { val: 5, dest: five_a },
            Instr::BinaryOperation {
                operator: BinaryOperator::Divide,
                lhs: four_b,
                rhs: five_b,
                dest: prod_3_a,
                ..
            },
            Instr::BinaryOperation {
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
             prod_4_a == prod_4_b,
        "{:#?}",
        &proc.blocks[0].instrs[..],
    );
}

fn run_expr(code: &'static str) -> i64 {
    let (mut d1, mut d2) = (vec![], vec![]);
    let mut lexer = Lexer::new(code.chars().peekable(), 0, &mut d1).peekable();
    let mut parser = Parser::new(&mut lexer, &mut d2);
    let expr = parser.expr().unwrap();
    assert!(d1.is_empty(), "{:?}", d1);
    assert!(d2.is_empty(), "{:?}", d2);
    let proc = lower_expr(&expr);
    let proc = resolve(&proc);
    codegen::run_jit(&proc)
}

#[test]
fn codegen_basic_arithmetic() {
    assert_eq!(1 + 2 * 3 - 4 / 5, run_expr("1 + 2 * 3 - 4 / 5"));
}

#[test]
fn codegen_booleans() {
    for (code, expected) in [("1 <= 2", 1), ("!-2", 0), ("3 == 1 + 2", 1)] {
        assert_eq!(expected, run_expr(code), "{}", code);
    }
}

#[test]
fn codegen_if() {
    assert_eq!(42 + 1337, run_expr("42 + if 69 then 1337 else 42"));
}

#[test]
fn variable_shadowing() {
    let code = "
        a := 2;
        a := a + 2;
        return a;
    ";
    let (mut d1, mut d2) = (vec![], vec![]);
    let mut lexer = Lexer::new(code.chars().peekable(), 0, &mut d1).peekable();
    let mut parser = Parser::new(&mut lexer, &mut d2);
    let block = parser.file();
    assert!(d1.is_empty(), "{:?}", d1);
    assert!(d2.is_empty(), "{:?}", d2);
    let file = block.unwrap();
    let proc = lower_file(&file);
    let proc = resolve(&proc);
    assert_eq!(4, codegen::run_jit(&proc))
}

#[test]
fn parse_block() {
    let code = "{
        a := 2;
        print(a + a);
    }";
    let (mut d1, mut d2) = (vec![], vec![]);
    let mut lexer = Lexer::new(code.chars().peekable(), 0, &mut d1).peekable();
    let mut parser = Parser::new(&mut lexer, &mut d2);
    let block = parser.block();
    assert!(d1.is_empty(), "{:?}", d1);
    assert!(d2.is_empty(), "{:?}", d2);
    let block = block.unwrap();
    assert_matches!(
        &block.stmts[0],
        Stmt::VarDecl(VarDecl {
            ident: Ident { name, .. },
            value: Expr::Int(_, 2),
            ..
        }) if name == "a",
    );
    assert_matches!(
        &block.stmts[1],
        Stmt::Print(
            _,
            Expr::Grouping(Grouping {
                expr: box Expr::BinaryOperation(box BinaryOperation {
                    lhs: Expr::Ident(Ident { name: name1, .. }),
                    rhs: Expr::Ident(Ident { name: name2, .. }),
                    operator: BinaryOperator::Add,
                    ..
                }),
                ..
            }),
            _,
        ) if name1 == "a" && name2 == "a",
    );
}

#[test]
fn parse_proc() {
    let code = "
        add := proc(a, b) {
            return a + b;
        };
    ";
    let (mut d1, mut d2) = (vec![], vec![]);
    let mut lexer = Lexer::new(code.chars().peekable(), 0, &mut d1).peekable();
    let mut parser = Parser::new(&mut lexer, &mut d2);
    let file = parser.file();
    assert!(d1.is_empty(), "{:?}", d1);
    assert!(d2.is_empty(), "{:?}", d2);
    let file = file.unwrap();
    assert_eq!(file.stmts.len(), 1);
    assert_matches!(
        &file.stmts[0],
        Stmt::VarDecl(VarDecl {
            ident: Ident { name, .. },
            value: Expr::Proc(Proc { params, .. }),
            ..
        })
        if name == "add" && matches!(
            &params[..],
            [Ident { name: a, .. }, Ident { name: b, .. }]
            if a == "a" && b == "b"
        ),
    );
}
