#![feature(box_patterns, assert_matches)]

mod diagnostic;
mod irs;
mod stages;

pub use diagnostic::Diagnostic;
pub use irs::{bytecode, common, syntax_tree, tokens};
pub use stages::{codegen, compilation, lexing, parsing};

#[cfg(test)]
mod tests;
