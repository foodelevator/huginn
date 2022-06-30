use std::fmt;
use std::ops::{BitOr, Range};

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

    pub fn range(self) -> Range<usize> {
        Range {
            start: self.start,
            end: self.end,
        }
    }

    pub fn display<'c>(&self, code: &'c str) -> DisplaySpan<'_, 'c> {
        DisplaySpan { span: self, code }
    }
}

impl BitOr for Span {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self {
        Self::new(self.start.min(rhs.start)..self.end.max(rhs.end))
    }
}

pub struct DisplaySpan<'s, 'c> {
    span: &'s Span,
    code: &'c str,
}

impl fmt::Display for DisplaySpan<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut line = String::new();
        let mut line_number = 1;
        let mut begin_col = 0;
        let mut end_col = 0;

        let mut mode = 0;
        for (i, c) in self.code.chars().enumerate() {
            match mode {
                0 => {
                    if c == '\n' {
                        line.clear();
                        line_number += 1;
                    } else {
                        line.push(c);
                        begin_col += 1;
                    }
                    if i == self.span.start {
                        end_col = begin_col;
                        mode = 1;
                    }
                }
                1 => {
                    line.push(c);
                    end_col += 1;
                    if i == self.span.end {
                        mode = 2
                    }
                }
                2 => {
                    if c == '\n' {
                        break;
                    } else {
                        line.push(c);
                    }
                }
                _ => unreachable!(),
            }
        }
        writeln!(f, "  --> <input>:{}:{}", line_number, begin_col)?;
        writeln!(f, "   |")?;
        writeln!(f, "{: >2} | {}", line_number, line)?;

        write!(f, "   |")?;
        for _ in 0..begin_col {
            write!(f, " ")?;
        }
        write!(f, "\x1b[31;1m")?;
        for _ in begin_col..end_col {
            write!(f, "^")?;
        }
        writeln!(f, "\x1b[0m")
    }
}
