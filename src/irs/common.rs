use std::ops::Range;

pub type Span = Range<usize>;

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Multiply,
    Subtract,
    Divide,
}
