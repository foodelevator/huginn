use std::fmt;

use crate::common::{FileId, Span};

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

    pub fn display<'c, 'f, F: Fn(FileId) -> &'f str>(
        &self,
        code: &'c str,
        filename: F,
    ) -> DisplayDiagnostic<'_, 'c, 'f, F> {
        DisplayDiagnostic {
            diagnostic: self,
            code,
            filename,
        }
    }
}

pub struct DisplayDiagnostic<'d, 'c, 'f, F: Fn(FileId) -> &'f str> {
    diagnostic: &'d Diagnostic,
    code: &'c str,
    filename: F,
}

impl<'f, F: Fn(FileId) -> &'f str> fmt::Display for DisplayDiagnostic<'_, '_, 'f, F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let d = self.diagnostic;
        write!(
            f,
            "{}: {}\n{}",
            d.level,
            d.message,
            d.location.display(self.code, &self.filename)
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
