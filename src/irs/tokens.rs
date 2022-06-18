use crate::common::Span;

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenKind {
    Int(i64),
    Plus,
    Asterix,
    LeftParen,
    RightParen,
}
