#![cfg_attr(test, feature(box_patterns, assert_matches))]

mod diagnostic;
mod irs;
mod stages;

pub use diagnostic::Diagnostic;
pub use irs::{bitcode, bytecode, common, syntax_tree, tokens};
pub use stages::{codegen, lexing, link, lowering, parsing, resolution};

#[cfg(test)]
mod tests;
