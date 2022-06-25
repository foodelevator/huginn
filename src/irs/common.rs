use std::fmt;
use std::marker::Copy;
use std::ops::{Deref, Range};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(range: Range<usize>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }

    pub fn single(start: usize) -> Self {
        Self {
            start,
            end: start + 1,
        }
    }

    pub fn unknown() -> Self {
        Self::new(0..0)
    }

    pub fn pos(self) -> Range<usize> {
        Range {
            start: self.start,
            end: self.end,
        }
    }
}

impl Deref for Span {
    type Target = Range<usize>;

    fn deref(&self) -> &Self::Target {
        unsafe { std::mem::transmute(self) }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Multiply,
    Subtract,
    Divide,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperator {
    Not,
    Negate,
}
