#![cfg_attr(test, feature(box_patterns, assert_matches))]

mod array;
mod diagnostic;
mod irs;
mod stages;

pub use array::Array;
pub use diagnostic::Diagnostic;
pub use irs::{bitcode, bytecode, common, syntax_tree, tokens};
pub use stages::{analysis, codegen, lexing, link, lowering, parsing};

#[cfg(test)]
mod tests;
