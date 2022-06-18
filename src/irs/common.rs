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

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Multiply,
    Subtract,
    Divide,
    Less,
    Greater,
}
