use std::fmt;

use crate::common::Span;

#[derive(Debug)]
pub struct Diagnostic {
    pub location: Span,
    pub level: Level,
    pub message: &'static str,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Level {
    Error,
    Warning,
}

impl Diagnostic {
    pub fn new(location: Span, level: Level, message: &'static str) -> Self {
        Self {
            location,
            level,
            message,
        }
    }

    pub fn error(location: Span, message: &'static str) -> Self {
        Self {
            location,
            level: Level::Error,
            message,
        }
    }

    pub fn warning(location: Span, message: &'static str) -> Self {
        Self {
            location,
            level: Level::Warning,
            message,
        }
    }

    pub fn display<'c>(&self, code: &'c str) -> DisplayDiagnostic<'_, 'c> {
        DisplayDiagnostic {
            diagnostic: self,
            code,
        }
    }
}

pub struct DisplayDiagnostic<'d, 'c> {
    diagnostic: &'d Diagnostic,
    code: &'c str,
}

impl fmt::Display for DisplayDiagnostic<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let d = self.diagnostic;
        write!(
            f,
            "{}: {}\n{}",
            d.level,
            d.message,
            d.location.display(self.code)
        )
    }
}

impl fmt::Display for Level {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Error => "\x1b[31;1merror\x1b[0m",
                Self::Warning => "\x1b[33;1mwarning\x1b[0m",
            }
        )
    }
}
