use crate::common::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Plus,
    Asterix,
    Minus,
    Slash,
    Bang,
    Equal,
    Semicolon,

    LeftParen,
    RightParen,
    LeftCurly,
    RightCurly,
    Less,
    Greater,

    BangEqual,
    LessEqual,
    GreaterEqual,

    LeftArrow,

    If,
    Then,
    Else,
    Let,

    Ident(String),

    Int(i64),
}
