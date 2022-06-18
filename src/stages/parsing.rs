use std::iter::Peekable;

use crate::{
    common::{BinaryOperator, Span, UnaryOperator},
    syntax_tree::{BinaryOperation, Expr, Grouping, If, UnaryOperation},
    tokens::{Token, TokenKind},
};

pub fn parse_expr(input: &mut Peekable<impl Iterator<Item = Token>>) -> Expr {
    parse_comparison(input)
}

macro_rules! assert_kind {
    ($token:expr, $kind:pat) => {
        match $token {
            Some(Token { span, kind: $kind }) => span,
            _ => panic!(),
        }
    };
}

fn parse_comparison(input: &mut Peekable<impl Iterator<Item = Token>>) -> Expr {
    parse_left_associative_binary_operation(input, parse_terms, |kind| match kind {
        TokenKind::Equal => Some(BinaryOperator::Equal),
        TokenKind::BangEqual => Some(BinaryOperator::NotEqual),
        TokenKind::Less => Some(BinaryOperator::Less),
        TokenKind::LessEqual => Some(BinaryOperator::LessEqual),
        TokenKind::Greater => Some(BinaryOperator::Greater),
        TokenKind::GreaterEqual => Some(BinaryOperator::GreaterEqual),
        _ => None,
    })
}

fn parse_terms(input: &mut Peekable<impl Iterator<Item = Token>>) -> Expr {
    parse_left_associative_binary_operation(input, parse_factors, |kind| match kind {
        TokenKind::Plus => Some(BinaryOperator::Add),
        TokenKind::Minus => Some(BinaryOperator::Subtract),
        _ => None,
    })
}

fn parse_factors(input: &mut Peekable<impl Iterator<Item = Token>>) -> Expr {
    parse_left_associative_binary_operation(input, parse_unary_operation, |token| match token {
        TokenKind::Asterix => Some(BinaryOperator::Multiply),
        TokenKind::Slash => Some(BinaryOperator::Divide),
        _ => None,
    })
}

fn parse_unary_operation(input: &mut Peekable<impl Iterator<Item = Token>>) -> Expr {
    match input.peek() {
        Some(&Token {
            span,
            kind: TokenKind::Bang,
        }) => {
            input.next();
            Expr::UnaryOperation(Box::new(UnaryOperation {
                op_span: span,
                operator: UnaryOperator::Not,
                operand: parse_unary_operation(input),
            }))
        }
        Some(&Token {
            span,
            kind: TokenKind::Minus,
        }) => {
            input.next();
            Expr::UnaryOperation(Box::new(UnaryOperation {
                op_span: span,
                operator: UnaryOperator::Negate,
                operand: parse_unary_operation(input),
            }))
        }
        _ => parse_innermost(input),
    }
}

fn parse_innermost(input: &mut Peekable<impl Iterator<Item = Token>>) -> Expr {
    match input.next() {
        Some(Token {
            span: if_span,
            kind: TokenKind::If,
        }) => finish_if(input, if_span),
        Some(Token {
            span: left_paren,
            kind: TokenKind::LeftParen,
        }) => finish_grouping(input, left_paren),
        Some(Token {
            span,
            kind: TokenKind::Int(val),
        }) => Expr::Int(span, val),
        _ => todo!(),
    }
}

fn finish_if(input: &mut Peekable<impl Iterator<Item = Token>>, if_span: Span) -> Expr {
    let cond = parse_expr(input);
    assert_kind!(input.next(), TokenKind::LeftCurly);
    let then = parse_expr(input);
    assert_kind!(input.next(), TokenKind::RightCurly);
    assert_kind!(input.next(), TokenKind::Else);
    assert_kind!(input.next(), TokenKind::LeftCurly);
    let else_ = parse_expr(input);
    assert_kind!(input.next(), TokenKind::RightCurly);

    Expr::If(Box::new(If {
        if_span,
        cond,
        then,
        else_,
    }))
}

fn finish_grouping(input: &mut Peekable<impl Iterator<Item = Token>>, left_paren: Span) -> Expr {
    let expr = Box::new(parse_expr(input));
    let right_paren = assert_kind!(input.next(), TokenKind::RightParen);
    Expr::Grouping(Grouping {
        left_paren,
        expr,
        right_paren,
    })
}

fn parse_left_associative_binary_operation<
    F: Fn(&mut Peekable<I>) -> Expr,
    I: Iterator<Item = Token>,
    M: Fn(&TokenKind) -> Option<BinaryOperator>,
>(
    input: &mut Peekable<I>,
    inner: F,
    matcher: M,
) -> Expr {
    let mut lhs = inner(input);
    while let Some(Token { span, kind }) = input.peek() {
        let op_span = *span;
        let operator = match matcher(kind) {
            Some(op) => op,
            None => break,
        };
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
