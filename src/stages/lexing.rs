use std::iter::Peekable;

use crate::{
    common::Span,
    tokens::{Token, TokenKind},
    Diagnostic,
};

pub struct Lexer<'d, I: Iterator<Item = char>> {
    input: Peekable<I>,
    index: usize,
    diagnostics: &'d mut Vec<Diagnostic>,
}

impl<'d, I: Iterator<Item = char>> Iterator for Lexer<'d, I> {
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

impl<'d, I: Iterator<Item = char>> Lexer<'d, I> {
    pub fn new(input: Peekable<I>, diagnostics: &'d mut Vec<Diagnostic>) -> Self {
        Self {
            input,
            index: 0,
            diagnostics,
        }
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        self.diagnostics.as_ref()
    }

    fn advance(&mut self) -> Option<Token> {
        let start = self.index;
        let kind = match self.next_char()? {
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '*' => TokenKind::Asterix,
            '/' => TokenKind::Slash,
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            '{' => TokenKind::LeftCurly,
            '}' => TokenKind::RightCurly,
            '=' if self.next_if_eq('=').is_some() => TokenKind::EqualEqual,
            '=' => TokenKind::Equal,
            '!' if self.next_if_eq('=').is_some() => TokenKind::BangEqual,
            '!' => TokenKind::Bang,
            '<' if self.next_if_eq('=').is_some() => TokenKind::LessEqual,
            '<' => TokenKind::Less,
            '>' if self.next_if_eq('=').is_some() => TokenKind::GreaterEqual,
            '>' => TokenKind::Greater,
            ':' if self.next_if_eq('=').is_some() => TokenKind::ColonEqual,
            ';' => TokenKind::Semicolon,
            ws if ws.is_whitespace() => return None,
            d @ '0'..='9' => self.lex_number(d),
            c if c.is_alphabetic() => self.lex_word(c),
            c => {
                self.unexpected_char(c);
                return None;
            }
        };

        Some(Token {
            span: Span::new(start..self.index),
            kind,
        })
    }

    fn lex_word(&mut self, first: char) -> TokenKind {
        let mut word = String::from(first);

        loop {
            match self.peek_char() {
                Some(c) if c.is_alphanumeric() => {
                    self.next_char();
                    word.push(c);
                }
                _ => break,
            }
        }

        match &*word {
            "if" => TokenKind::If,
            "then" => TokenKind::Then,
            "else" => TokenKind::Else,
            "print" => TokenKind::Print,

            _ => TokenKind::Ident(word),
        }
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

    fn next_if_eq(&mut self, c: char) -> Option<char> {
        self.next_if(|ch| c == ch)
    }

    fn next_if(&mut self, p: impl Fn(char) -> bool) -> Option<char> {
        let ch = self.peek_char()?;
        if p(ch) {
            self.next_char()
        } else {
            None
        }
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

    fn unexpected_char(&mut self, _c: char) {
        self.diagnostics.push(Diagnostic::error(
            Span::single(self.index),
            "Unexpected char",
        ))
    }
}
