use std::iter::Peekable;

use crate::Span;

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

pub struct Lexer<I: Iterator<Item = char>> {
    input: Peekable<I>,
    index: usize,
}

impl<I: Iterator<Item = char>> Lexer<I> {
    pub fn new(input: Peekable<I>) -> Self {
        Self { input, index: 0 }
    }

    fn advance(&mut self) -> Option<Token> {
        let start = self.index;
        let kind = match self.next_char()? {
            '+' => Some(TokenKind::Plus),
            '*' => Some(TokenKind::Asterix),
            '(' => Some(TokenKind::LeftParen),
            ')' => Some(TokenKind::RightParen),
            ws if ws.is_whitespace() => None,
            d @ '0'..='9' => {
                let mut v = d as i64 - '0' as i64;

                // FIXME: handle overflows
                while let Some(d @ ('0'..='9' | '_')) = self.peek_char() {
                    self.next_char();
                    if matches!(d, '0'..='9') {
                        v *= 10;
                        v += d as i64 - '0' as i64;
                    }
                }

                Some(TokenKind::Int(v))
            }
            c => todo!("Found not yet lexable char {}", c),
        }?;

        Some(Token {
            span: start..self.index,
            kind,
        })
    }

    fn peek_char(&mut self) -> Option<char> {
        self.input.peek().copied()
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.input.next();
        if c.is_some() {
            self.index += 1
        }
        c
    }
}

impl<I: Iterator<Item = char>> Iterator for Lexer<I> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(token) = self.advance() {
                return Some(token);
            }
            self.input.peek()?;
        }
    }
}
