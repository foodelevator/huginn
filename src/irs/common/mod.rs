mod span;

pub use span::Span;

pub type FileId = u32;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub span: Span,
    pub name: String,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Multiply,
    Subtract,
    Divide,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperator {
    Not,
    Negate,
}
