use std::iter::Peekable;

use crate::{
    lexing::{Token, TokenKind},
    Span,
};

#[derive(Debug, Clone)]
pub enum Expr {
    Int(Span, i64),
    BinaryOperation(Box<BinaryOperation>),
    Grouping {
        left_paren: Span,
        expr: Box<Expr>,
        right_paren: Span,
    },
}

#[derive(Debug, Clone)]
pub struct BinaryOperation {
    pub lhs: Expr,
    pub rhs: Expr,
    pub op_span: Span,
    pub operator: BinaryOperator,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Plus,
    Times,
}

impl Expr {
    pub fn parse(input: &mut Peekable<impl Iterator<Item = Token>>) -> Self {
        parse_terms(input)
    }
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
            span,
            kind: TokenKind::Int(val),
        }) => Expr::Int(span, val),
        Some(Token {
            span: left_paren,
            kind: TokenKind::LeftParen,
        }) => {
            let expr = Box::new(Expr::parse(input));
            let right_paren = assert_kind!(input.next(), TokenKind::RightParen);
            Expr::Grouping {
                left_paren,
                expr,
                right_paren,
            }
        }
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
