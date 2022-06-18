use crate::common::Span;

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenKind {
    Plus,
    Asterix,
    Minus,
    Slash,

    LeftParen,
    RightParen,
    Less,
    Greater,

    Int(i64),
}
