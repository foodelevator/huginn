use std::iter::Peekable;

use crate::{
    common::{BinaryOperator, Span, UnaryOperator},
    syntax_tree::{BinaryOperation, Expr, Grouping, If, UnaryOperation},
    tokens::{Token, TokenKind},
    Diagnostic,
};

pub struct Parser<'i, 'd, I: Iterator<Item = Token>> {
    input: &'i mut Peekable<I>,
    diagnostics: &'d mut Vec<Diagnostic>,
}

macro_rules! assert_kind {
    ($self:expr, $kind:pat) => {
        match $self.input.next() {
            Some(Token { span, kind: $kind }) => span,
            Some(token) => {
                let span = token.span;
                $self.unexpected_token(token);
                span
            }
            None => {
                $self.unexpected_eof();
                Span::unknown()
            }
        }
    };
}

impl<'i, 'd, I: Iterator<Item = Token>> Parser<'i, 'd, I> {
    pub fn new(input: &'i mut Peekable<I>, diagnostics: &'d mut Vec<Diagnostic>) -> Self {
        Self { input, diagnostics }
    }

    pub fn expr(&mut self) -> Expr {
        self.comparison()
    }

    fn comparison(&mut self) -> Expr {
        self.parse_left_associative_binary_operation(Self::terms, |kind| match kind {
            TokenKind::Equal => Some(BinaryOperator::Equal),
            TokenKind::BangEqual => Some(BinaryOperator::NotEqual),
            TokenKind::Less => Some(BinaryOperator::Less),
            TokenKind::LessEqual => Some(BinaryOperator::LessEqual),
            TokenKind::Greater => Some(BinaryOperator::Greater),
            TokenKind::GreaterEqual => Some(BinaryOperator::GreaterEqual),
            _ => None,
        })
    }

    fn terms(&mut self) -> Expr {
        self.parse_left_associative_binary_operation(Self::factors, |kind| match kind {
            TokenKind::Plus => Some(BinaryOperator::Add),
            TokenKind::Minus => Some(BinaryOperator::Subtract),
            _ => None,
        })
    }

    fn factors(&mut self) -> Expr {
        self.parse_left_associative_binary_operation(Self::unary_operation, |token| match token {
            TokenKind::Asterix => Some(BinaryOperator::Multiply),
            TokenKind::Slash => Some(BinaryOperator::Divide),
            _ => None,
        })
    }

    fn unary_operation(&mut self) -> Expr {
        match self.input.peek() {
            Some(&Token {
                span,
                kind: TokenKind::Bang,
            }) => {
                self.input.next();
                Expr::UnaryOperation(Box::new(UnaryOperation {
                    op_span: span,
                    operator: UnaryOperator::Not,
                    operand: self.unary_operation(),
                }))
            }
            Some(&Token {
                span,
                kind: TokenKind::Minus,
            }) => {
                self.input.next();
                Expr::UnaryOperation(Box::new(UnaryOperation {
                    op_span: span,
                    operator: UnaryOperator::Negate,
                    operand: self.unary_operation(),
                }))
            }
            _ => self.innermost(),
        }
    }

    fn innermost(&mut self) -> Expr {
        match self.input.next() {
            Some(Token {
                span: if_span,
                kind: TokenKind::If,
            }) => self.finish_if(if_span),
            Some(Token {
                span: left_paren,
                kind: TokenKind::LeftParen,
            }) => self.finish_grouping(left_paren),
            Some(Token {
                span,
                kind: TokenKind::Int(val),
            }) => Expr::Int(span, val),
            Some(token) => self.unexpected_token(token),
            None => self.unexpected_eof(),
        }
    }

    fn finish_if(&mut self, if_span: Span) -> Expr {
        let cond = self.expr();
        assert_kind!(self, TokenKind::LeftCurly);
        let then = self.expr();
        assert_kind!(self, TokenKind::RightCurly);
        assert_kind!(self, TokenKind::Else);
        assert_kind!(self, TokenKind::LeftCurly);
        let else_ = self.expr();
        assert_kind!(self, TokenKind::RightCurly);

        Expr::If(Box::new(If {
            if_span,
            cond,
            then,
            else_,
        }))
    }

    fn finish_grouping(&mut self, left_paren: Span) -> Expr {
        let expr = Box::new(self.expr());
        let right_paren = assert_kind!(self, TokenKind::RightParen);
        Expr::Grouping(Grouping {
            left_paren,
            expr,
            right_paren,
        })
    }

    fn parse_left_associative_binary_operation<
        F: Fn(&mut Self) -> Expr,
        M: Fn(&TokenKind) -> Option<BinaryOperator>,
    >(
        &mut self,
        inner: F,
        matcher: M,
    ) -> Expr {
        let mut lhs = inner(self);
        while let Some(Token { span, kind }) = self.input.peek() {
            let op_span = *span;
            let operator = match matcher(kind) {
                Some(op) => op,
                None => break,
            };
            self.input.next();
            lhs = Expr::BinaryOperation(Box::new(BinaryOperation {
                lhs,
                rhs: inner(self),
                op_span,
                operator,
            }));
        }
        lhs
    }

    fn unexpected_token(&mut self, t: Token) -> Expr {
        self.diagnostics
            .push(Diagnostic::error(t.span, "Unexpected token"));
        Expr::Error
    }

    fn unexpected_eof(&mut self) -> Expr {
        self.diagnostics.push(Diagnostic::error(
            Span::unknown(),
            "Expected token, found EOF",
        ));
        Expr::Error
    }
}
