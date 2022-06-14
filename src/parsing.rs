use std::iter::Peekable;

use crate::{
    lexing::{Token, TokenKind},
    Span,
};

#[derive(Debug)]
pub enum Expr {
    Int(Span, usize),
    Plus {
        lhs: Box<Expr>,
        plus: Span,
        rhs: Box<Expr>,
    },
    Times {
        lhs: Box<Expr>,
        asterix: Span,
        rhs: Box<Expr>,
    },
    Grouping {
        left_paren: Span,
        expr: Box<Expr>,
        right_paren: Span,
    },
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
        |lhs, plus, rhs| Expr::Plus { lhs, plus, rhs },
    )
}

fn parse_factors(input: &mut Peekable<impl Iterator<Item = Token>>) -> Expr {
    parse_left_associative_binary_operation(
        input,
        parse_innermost,
        TokenKind::Asterix,
        |lhs, asterix, rhs| Expr::Times { lhs, asterix, rhs },
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
    combine: impl Fn(Box<Expr>, Span, Box<Expr>) -> Expr,
) -> Expr {
    let mut lhs = inner(input);
    while let Some(Token { span, kind }) = input.peek() {
        if *kind != middle {
            break;
        }
        let span = span.clone();
        input.next();
        lhs = combine(Box::new(lhs), span, Box::new(inner(input)));
    }
    lhs
}
