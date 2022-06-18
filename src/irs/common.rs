use std::ops::Range;

pub type Span = Range<usize>;

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Plus,
    Times,
}
