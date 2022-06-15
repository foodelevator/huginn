use std::assert_matches::assert_matches;

use crate::{
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
fn parse_expressions() {
    let mut lexer = Lexer::new("1 + 2 * 3".chars().peekable()).peekable();
    assert_matches!(
        Expr::parse(&mut lexer),
        Expr::BinaryOperation(box BinaryOperation {
            operator: BinaryOperator::Plus,
            lhs: Expr::Int(_, 1),
            rhs: Expr::BinaryOperation(box BinaryOperation {
                lhs: Expr::Int(_, 2),
                rhs: Expr::Int(_, 3),
                operator: BinaryOperator::Times,
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
            operator: BinaryOperator::Plus,
            lhs: Expr::BinaryOperation(box BinaryOperation {
                lhs: Expr::Int(_, 1),
                rhs: Expr::Int(_, 2),
                operator: BinaryOperator::Times,
                ..
            }),
            rhs: Expr::Int(_, 3),
            ..
        })
    );
    assert_eq!(lexer.next(), None);
}
