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
    Bang,
    Equal,

    LeftParen,
    RightParen,
    LeftCurly,
    RightCurly,
    Less,
    Greater,

    BangEqual,
    LessEqual,
    GreaterEqual,

    If,
    Else,

    Int(i64),
}
