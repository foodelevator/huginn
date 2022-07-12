use crate::{
    common::{BinaryOperator, Ident, Span, UnaryOperator},
    syntax_tree::{
        Assign, BinaryOperation, Block, Expr, ExprStmt, File, Grouping, IfExpr, IfStmt, Proc, Stmt,
        UnaryOperation, VarDecl, While,
    },
    tokens::{Token, TokenKind},
    Diagnostic, lexing::Lexer,
};

pub struct Parser<'i, I: Iterator<Item = char>> {
    input: &'i mut Lexer<I>,
    diagnostics: Vec<Diagnostic>,
}

macro_rules! assert_next {
    ($self:expr, $kind:pat) => {
        match $self.input.next() {
            Some(Token { span, kind: $kind }) => Some(span),
            Some(token) => {
                $self.unexpected_token(Some(&token));
                None
            }
            None => {
                $self.unexpected_token(None);
                None
            }
        }
    };
}

impl<'i, I: Iterator<Item = char>> Parser<'i, I> {
    pub fn new(input: &'i mut Lexer<I>) -> Self {
        Self { input, diagnostics: Vec::new() }
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        self.diagnostics.as_ref()
    }

    pub fn file(&mut self) -> Option<File> {
        let mut stmts = Vec::new();
        while self.input.peek().is_some() {
            stmts.push(self.stmt()?);
        }
        Some(File { stmts })
    }

    pub fn block(&mut self) -> Option<Block> {
        let left_curly = assert_next!(self, TokenKind::LeftCurly)?;
        let mut stmts = Vec::new();
        loop {
            match self.input.peek() {
                Some(Token {
                    kind: TokenKind::RightCurly,
                    ..
                }) => break,
                _ => {
                    stmts.push(self.stmt()?);
                }
            }
        }
        let right_curly = assert_next!(self, TokenKind::RightCurly)?;
        Some(Block {
            left_curly,
            stmts,
            right_curly,
        })
    }

    pub fn stmt(&mut self) -> Option<Stmt> {
        match self.input.peek() {
            Some(&Token {
                kind: TokenKind::If,
                span,
            }) => {
                self.input.next();
                return self.finish_if_stmt(span);
            }
            Some(&Token {
                kind: TokenKind::While,
                span,
            }) => {
                self.input.next();
                return self.finish_while(span);
            }
            Some(&Token {
                kind: TokenKind::Print,
                span: print_span,
            }) => {
                self.input.next();
                let left_paren = assert_next!(self, TokenKind::LeftParen)?;
                let expr = self.finish_grouping(left_paren)?;
                let semicolon = assert_next!(self, TokenKind::Semicolon)?;
                return Some(Stmt::Print(print_span, expr, semicolon));
            }
            Some(&Token {
                kind: TokenKind::Return,
                span: return_span,
            }) => {
                self.input.next();
                let expr = self.expr()?;
                let semicolon = assert_next!(self, TokenKind::Semicolon)?;
                return Some(Stmt::Return(return_span, expr, semicolon));
            }
            _ => {}
        }
        let expr = self.expr()?;
        match self.input.peek() {
            Some(&Token {
                kind: TokenKind::Equal,
                span: assign_sign,
            }) => {
                self.input.next();
                let value = self.expr()?;
                let semicolon = assert_next!(self, TokenKind::Semicolon)?;
                Some(Stmt::Assign(Assign {
                    assignee: expr,
                    assign_sign,
                    value,
                    semicolon,
                }))
            }
            Some(&Token {
                kind: TokenKind::ColonEqual,
                span: decl_sign,
            }) => {
                self.input.next();

                let ident = match expr {
                    Expr::Ident(ident) => ident,
                    expr => {
                        self.invalid_variable_ident(expr.span());
                        return None;
                    }
                };
                let value = self.expr()?;
                let semicolon = assert_next!(self, TokenKind::Semicolon)?;
                Some(Stmt::VarDecl(VarDecl {
                    ident,
                    decl_sign,
                    value,
                    semicolon,
                }))
            }
            Some(&Token {
                kind: TokenKind::Semicolon,
                span: semicolon,
            }) => {
                self.input.next();
                Some(Stmt::Expr(ExprStmt { expr, semicolon }))
            }
            _ => {
                let token = self.input.next();
                self.unexpected_token(token.as_ref());
                None
            }
        }
    }

    fn finish_if_stmt(&mut self, if_span: Span) -> Option<Stmt> {
        let cond = self.expr()?;
        let then = self.block()?;
        let else_ = match self.input.peek() {
            Some(&Token {
                kind: TokenKind::Else,
                span,
            }) => {
                self.input.next();
                let else_ = self.block()?;
                Some((span, else_))
            }
            _ => None,
        };
        Some(Stmt::If(IfStmt {
            if_span,
            cond,
            then,
            else_,
        }))
    }

    pub fn finish_while(&mut self, while_span: Span) -> Option<Stmt> {
        let cond = self.expr()?;
        let body = self.block()?;
        Some(Stmt::While(While {
            while_span,
            cond,
            body,
        }))
    }

    pub fn expr(&mut self) -> Option<Expr> {
        self.comparison()
    }

    fn comparison(&mut self) -> Option<Expr> {
        self.parse_left_associative_binary_operation(Self::terms, |kind| match kind {
            TokenKind::EqualEqual => Some(BinaryOperator::Equal),
            TokenKind::BangEqual => Some(BinaryOperator::NotEqual),
            TokenKind::Less => Some(BinaryOperator::Less),
            TokenKind::LessEqual => Some(BinaryOperator::LessEqual),
            TokenKind::Greater => Some(BinaryOperator::Greater),
            TokenKind::GreaterEqual => Some(BinaryOperator::GreaterEqual),
            _ => None,
        })
    }

    fn terms(&mut self) -> Option<Expr> {
        self.parse_left_associative_binary_operation(Self::factors, |kind| match kind {
            TokenKind::Plus => Some(BinaryOperator::Add),
            TokenKind::Minus => Some(BinaryOperator::Subtract),
            _ => None,
        })
    }

    fn factors(&mut self) -> Option<Expr> {
        self.parse_left_associative_binary_operation(Self::unary_operation, |token| match token {
            TokenKind::Asterix => Some(BinaryOperator::Multiply),
            TokenKind::Slash => Some(BinaryOperator::Divide),
            _ => None,
        })
    }

    fn unary_operation(&mut self) -> Option<Expr> {
        match self.input.peek() {
            Some(&Token {
                span,
                kind: TokenKind::Bang,
            }) => {
                self.input.next();
                self.unary_operation().map(|operand| {
                    Expr::UnaryOperation(Box::new(UnaryOperation {
                        op_span: span,
                        operator: UnaryOperator::Not,
                        operand,
                    }))
                })
            }
            Some(&Token {
                span,
                kind: TokenKind::Minus,
            }) => {
                self.input.next();
                self.unary_operation().map(|operand| {
                    Expr::UnaryOperation(Box::new(UnaryOperation {
                        op_span: span,
                        operator: UnaryOperator::Negate,
                        operand,
                    }))
                })
            }
            _ => self.innermost(),
        }
    }

    fn innermost(&mut self) -> Option<Expr> {
        match self.input.next() {
            Some(Token {
                span: if_span,
                kind: TokenKind::If,
            }) => self.finish_if_expr(if_span),
            Some(Token {
                span: left_paren,
                kind: TokenKind::LeftParen,
            }) => self.finish_grouping(left_paren),
            Some(Token {
                span,
                kind: TokenKind::Int(val),
            }) => Some(Expr::Int(span, val)),
            Some(Token {
                span,
                kind: TokenKind::Ident(name),
            }) => Some(Expr::Ident(Ident { span, name })),
            Some(Token {
                span,
                kind: TokenKind::Proc,
            }) => self.finish_proc(span),
            token => {
                self.unexpected_token(token.as_ref());
                None
            }
        }
    }

    fn finish_if_expr(&mut self, if_span: Span) -> Option<Expr> {
        let cond = self.expr()?;
        let then_span = assert_next!(self, TokenKind::Then)?;
        let then = self.expr()?;
        let else_span = assert_next!(self, TokenKind::Else)?;
        let else_ = self.expr()?;

        Some(Expr::If(Box::new(IfExpr {
            if_span,
            cond,
            then_span,
            then,
            else_span,
            else_,
        })))
    }

    fn finish_grouping(&mut self, left_paren: Span) -> Option<Expr> {
        let expr = Box::new(self.expr()?);
        let right_paren = assert_next!(self, TokenKind::RightParen)?;
        Some(Expr::Grouping(Grouping {
            left_paren,
            expr,
            right_paren,
        }))
    }

    fn finish_proc(&mut self, proc_span: Span) -> Option<Expr> {
        assert_next!(self, TokenKind::LeftParen)?;
        let mut params = vec![];
        let mut expect_ident = true;
        loop {
            match self.input.next() {
                Some(Token {
                    span,
                    kind: TokenKind::Ident(name),
                }) if expect_ident => {
                    params.push(Ident { span, name });
                    expect_ident = false;
                }
                Some(Token {
                    kind: TokenKind::Comma,
                    ..
                }) if !expect_ident => {
                    expect_ident = true;
                }
                Some(Token {
                    kind: TokenKind::RightParen,
                    ..
                }) => break,
                token => {
                    self.unexpected_token(token.as_ref());
                    return None;
                }
            }
        }
        let body = self.block()?;

        Some(Expr::Proc(Proc {
            proc_span,
            params,
            body,
        }))
    }

    fn parse_left_associative_binary_operation<
        F: Fn(&mut Self) -> Option<Expr>,
        M: Fn(&TokenKind) -> Option<BinaryOperator>,
    >(
        &mut self,
        inner: F,
        matcher: M,
    ) -> Option<Expr> {
        let mut lhs = inner(self)?;
        while let Some(Token { span, kind }) = self.input.peek() {
            let op_span = *span;
            let operator = match matcher(kind) {
                Some(op) => op,
                None => break,
            };
            self.input.next();
            let rhs = inner(self)?;
            lhs = Expr::BinaryOperation(Box::new(BinaryOperation {
                lhs,
                rhs,
                op_span,
                operator,
            }));
        }
        Some(lhs)
    }

    fn unexpected_token(&mut self, token: Option<&Token>) {
        if let Some(t) = token {
            self.diagnostics
                .push(Diagnostic::error(t.span, "Unexpected token"));
        } else {
            self.diagnostics.push(Diagnostic::error(
                Span::unknown(),
                "Expected token, found EOF",
            ));
        }
    }

    fn invalid_variable_ident(&mut self, location: Span) {
        self.diagnostics
            .push(Diagnostic::error(location, "Invalid variable identifier"));
    }
}
