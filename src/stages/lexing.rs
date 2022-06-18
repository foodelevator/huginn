use std::iter::Peekable;

use crate::tokens::{Token, TokenKind};

pub struct Lexer<I: Iterator<Item = char>> {
    input: Peekable<I>,
    index: usize,
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

impl<I: Iterator<Item = char>> Lexer<I> {
    pub fn new(input: Peekable<I>) -> Self {
        Self { input, index: 0 }
    }

    fn advance(&mut self) -> Option<Token> {
        let start = self.index;
        let kind = match self.next_char()? {
            '+' => Some(TokenKind::Plus),
            '-' => Some(TokenKind::Minus),
            '*' => Some(TokenKind::Asterix),
            '/' => Some(TokenKind::Slash),
            '(' => Some(TokenKind::LeftParen),
            ')' => Some(TokenKind::RightParen),
            ws if ws.is_whitespace() => None,
            d @ '0'..='9' => Some(self.lex_number(d)),
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

    fn lex_number(&mut self, first: char) -> TokenKind {
        let mut base = 10;
        if first == '0' {
            match self.peek_char() {
                Some('x') => {
                    base = 0x10;
                    self.next_char();
                }
                Some('b') => {
                    base = 0b10;
                    self.next_char();
                }
                _ => {}
            }
        }

        let mut value = first as i64 - '0' as i64;

        // FIXME: handle overflows
        loop {
            let v = match self.peek_char() {
                Some(d @ '0'..='9') if d as i64 - ('0' as i64) < base => d as i64 - '0' as i64,
                Some(d @ 'a'..='f') if d as i64 - 'a' as i64 + 0xa < base => {
                    d as i64 - '0' as i64 + 0xa
                }
                Some('_') => {
                    self.next_char();
                    continue;
                }
                _ => break,
            };
            self.next_char();
            value *= base;
            value += v;
        }

        TokenKind::Int(value)
    }
}
