use std::iter::Peekable;

use crate::{
    common::BinaryOperator,
    syntax_tree::{BinaryOperation, Expr},
    tokens::{Token, TokenKind},
};

pub fn parse_expr(input: &mut Peekable<impl Iterator<Item = Token>>) -> Expr {
    parse_terms(input)
}

macro_rules! assert_kind {
    ($token:expr, $kind:pat) => {
        match $token {
            Some(Token { span, kind: $kind }) => span,
            _ => todo!(),
        }
    };
}

fn parse_terms(input: &mut Peekable<impl Iterator<Item = Token>>) -> Expr {
    parse_left_associative_binary_operation(
        input,
        parse_factors,
        TokenKind::Plus,
        BinaryOperator::Plus,
    )
}

fn parse_factors(input: &mut Peekable<impl Iterator<Item = Token>>) -> Expr {
    parse_left_associative_binary_operation(
        input,
        parse_innermost,
        TokenKind::Asterix,
        BinaryOperator::Times,
    )
}

fn parse_innermost(input: &mut Peekable<impl Iterator<Item = Token>>) -> Expr {
    match input.next() {
        Some(Token {
            span: left_paren,
            kind: TokenKind::LeftParen,
        }) => {
            let expr = Box::new(parse_expr(input));
            let right_paren = assert_kind!(input.next(), TokenKind::RightParen);
            Expr::Grouping {
                left_paren,
                expr,
                right_paren,
            }
        }
        Some(Token {
            span,
            kind: TokenKind::Int(val),
        }) => Expr::Int(span, val),
        _ => todo!(),
    }
}

fn parse_left_associative_binary_operation<
    F: Fn(&mut Peekable<I>) -> Expr,
    I: Iterator<Item = Token>,
>(
    input: &mut Peekable<I>,
    inner: F,
    middle: TokenKind,
    operator: BinaryOperator,
) -> Expr {
    let mut lhs = inner(input);
    while let Some(Token { span, kind }) = input.peek() {
        if *kind != middle {
            break;
        }
        let op_span = span.clone();
        input.next();
        lhs = Expr::BinaryOperation(Box::new(BinaryOperation {
            lhs,
            rhs: inner(input),
            op_span,
            operator,
        }));
    }
    lhs
}
