use crate::common::{BinaryOperator, Ident, Span, UnaryOperator};

#[derive(Debug, Clone)]
pub struct File {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub left_curly: Span,
    pub stmts: Vec<Stmt>,
    pub right_curly: Span,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(ExprStmt),
    VarDecl(VarDecl),
    Assign(Assign),
    If(IfStmt),
    While(While),
    Print(Span, Expr, Span),
    Return(Span, Expr, Span),
}

#[derive(Debug, Clone)]
pub struct ExprStmt {
    pub expr: Expr,
    pub semicolon: Span,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub ident: Ident,
    pub decl_sign: Span,
    pub value: Expr,
    pub semicolon: Span,
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub assignee: Expr,
    pub assign_sign: Span,
    pub value: Expr,
    pub semicolon: Span,
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub if_span: Span,
    pub cond: Expr,
    pub then: Block,
    pub else_: Option<(Span, Block)>,
}

#[derive(Debug, Clone)]
pub struct While {
    pub while_span: Span,
    pub cond: Expr,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Grouping(Grouping),
    Int(Span, i64),
    BinaryOperation(Box<BinaryOperation>),
    UnaryOperation(Box<UnaryOperation>),
    If(Box<IfExpr>),
    Ident(Ident),
    Proc(Proc),
}

#[derive(Debug, Clone)]
pub struct Grouping {
    pub left_paren: Span,
    pub expr: Box<Expr>,
    pub right_paren: Span,
}

#[derive(Debug, Clone)]
pub struct BinaryOperation {
    pub lhs: Expr,
    pub rhs: Expr,
    pub op_span: Span,
    pub operator: BinaryOperator,
}

#[derive(Debug, Clone)]
pub struct UnaryOperation {
    pub op_span: Span,
    pub operand: Expr,
    pub operator: UnaryOperator,
}

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub if_span: Span,
    pub cond: Expr,
    pub then_span: Span,
    pub then: Expr,
    pub else_span: Span,
    pub else_: Expr,
}

#[derive(Debug, Clone)]
pub struct Proc {
    pub proc_span: Span,
    pub params: Vec<Ident>,
    pub body: Block,
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            &Self::Grouping(Grouping {
                left_paren,
                right_paren,
                ..
            }) => left_paren | right_paren,
            &Self::Int(span, _) => span,
            Self::BinaryOperation(box BinaryOperation { lhs, rhs, .. }) => lhs.span() | rhs.span(),
            Self::UnaryOperation(box UnaryOperation {
                op_span, operand, ..
            }) => *op_span | operand.span(),
            Self::If(box IfExpr { if_span, else_, .. }) => *if_span | else_.span(),
            &Self::Ident(Ident { span, .. }) => span,
            Self::Proc(Proc {
                proc_span, body, ..
            }) => *proc_span | body.right_curly,
        }
    }
}
