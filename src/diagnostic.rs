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
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}: {}\n --> {}",
            self.level, self.message, self.location
        )
    }
}

impl fmt::Display for Level {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Error => "\x1b[31merror\x1b[0m",
                Self::Warning => "\x1b[33mwarning\x1b[0m",
            }
        )
    }
}
