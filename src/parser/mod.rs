//! The main parsing interface.

use crate::io::error::{ Result, CortexError };
use crate::lexer::Lexer;
use crate::ast::{
    AstNodeNew,
    Func,
    Expr,
    Ident,
    ExprKind,
    LitKind,
    StmtKind,
    IdentCtx,
    CondKind,
};
use crate::symbols::{
    Token,
    TokenKind,
    BinOpKind,
    UnaryOpKind,
    OpAssoc,
    TyKind,
    KwdKind,
    DelimKind,
    BraceKind,
    Literal,
    MaybeFrom,
};
use crate::sess::SessCtx;

/// The parser object.
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    /// The context of the current compilation session.
    ctx: &'a SessCtx,
    /// The current token.
    tok: Token,
    /// The previous token.
    prev_tok: Token,
}

impl<'a> Parser<'_> {
    /// Creates a new parser from a given context.
    pub fn new(ctx: &'a SessCtx) -> Result<Parser<'a>> {
        let mut lexer = Lexer::new(ctx);
        let tok = lexer.next_token()?;
        Ok(Parser {
            lexer,
            ctx,
            tok,
            prev_tok: Token::dummy(),
        })
    }

    /// The main driver of the parser. This method parses the given file and returns its Abstract Syntax Tree (AST).
    pub fn parse(&mut self) -> Result<Vec<Box<AstNodeNew>>> {
        let mut tree = Vec::new();
        while !self.tok.is_eof() {
            if let TokenKind::Kwd(kwd_kind) = &self.tok.kind {
                let child = match kwd_kind.clone() {
                    KwdKind::Include => 
                        AstNodeNew::Expr(self.parse_include()?),
                    KwdKind::Func =>
                        AstNodeNew::Func(self.parse_func()?),
                    _ => unimplemented!("error message for bad start keyword: '{}'", kwd_kind)
                };
                tree.push(Box::new(child));
            } else {
                return Err(CortexError::SyntaxError {
                    file_path: self.ctx.file_path(),
                    msg: format!("expected keyword but got '{}'", self.tok.value()),
                    span: self.tok.span,
                    help: None,
                }.into());
            }
        }
        Ok(tree)
    }

    /// Parses a compound expression, i.e., a collection of statements and/or expressions inside a
    /// set of curly braces, `{` and `}`.
    fn parse_compound(&mut self) -> Result<Expr> {
        let mut children = Vec::new();
        self.eat(TokenKind::BraceOpen(BraceKind::Curly))?;
        loop {
            let child = match self.tok.kind.clone() {
                TokenKind::Id(_) | TokenKind::Num(_) => {
                    let expr = self.parse_expr()?;
                    self.eat(TokenKind::Delim(DelimKind::Scolon))?;
                    AstNodeNew::Expr(expr)
                },
                TokenKind::Kwd(ref kwd_kind) => self.parse_kwd(kwd_kind)?,
                TokenKind::BraceClosed(BraceKind::Curly) => {
                    self.advance()?;
                    break;
                }
                _ => unimplemented!("DEV ERROR: NOT SURE HOW TO PROCEED ON TOKEN: {}", self.tok),
            };
            children.push(Box::new(child));
        }
        Ok(Expr::new(ExprKind::Compound(children)))
    }

    /// Parses an include statement, e.g., `include "some_file.cx"`.
    fn parse_include(&mut self) -> Result<Expr> {
        self.advance()?; // skip 'include' kwd
        if let TokenKind::String(_) = self.tok.kind {
            let incl = Expr::new(ExprKind::Stmt(
                StmtKind::Incl(self.tok.value())
            ));
            self.advance()?; // skip string
            self.eat(TokenKind::Delim(DelimKind::Scolon))?;
            Ok(incl)
        } else {
            Err(CortexError::SyntaxError {
                file_path: self.ctx.file_path(),
                msg: format!("expected string literal but got '{}'", self.tok.value()),
                span: self.tok.span,
                help: None,
            }.into())
        }
    }

    /// Continues parsing according to the keyword token the parser has encountered.
    ///
    /// # Examples
    /// If the parser encounters the `func` keyword, it will continue by attempting to parse a
    /// function.
    ///
    /// If the parser encounters the `if` keyword, it will continue by attempting to parse an
    /// if/else if/else statement.
    fn parse_kwd(&mut self, kwd_kind: &KwdKind) -> Result<AstNodeNew> {
        let node = match *kwd_kind {
            // early return for function
            KwdKind::Func => return Ok(AstNodeNew::Func(self.parse_func()?)),
            KwdKind::Include => self.parse_include()?,
            KwdKind::Let => self.parse_let()?,
            KwdKind::Ret => self.parse_ret()?,
            KwdKind::If => self.parse_if()?,
            KwdKind::Else => {
                self.advance()?;
                let (kind, span) = match self.tok.kind {
                    TokenKind::Kwd(KwdKind::If) => ("else if", self.prev_tok.span.to(self.tok.span)),
                    _ => ("else", self.prev_tok.span),
                };
                return Err(CortexError::SyntaxError {
                    file_path: self.ctx.file_path(),
                    msg: format!("found '{}' without a preceding 'if' statement", kind),
                    span,
                    help: None,
                }.into());
            }
            KwdKind::While => self.parse_while()?,
            KwdKind::For => unimplemented!("parsing of for loop"),
        };
        Ok(AstNodeNew::Expr(node))
    }

    /// Parses a while loop.
    fn parse_while(&mut self) -> Result<Expr> {
        self.advance()?; // skip 'while' kwd
        let expr = self.parse_expr()?;
        let body = self.parse_compound()?;
        Ok(Expr::new(ExprKind::Cond(
            CondKind::While(Box::new(expr), Box::new(body))
        )))
    }

    /// Parses an if/else if/else statement.
    fn parse_if(&mut self) -> Result<Expr> {
        self.advance()?; // skip 'if' kwd
        match self.tok.kind {
            TokenKind::BraceOpen(BraceKind::Curly) =>
                return Err(CortexError::SyntaxError {
                    file_path: self.ctx.file_path(),
                    msg: String::from("expected expression"),
                    span: self.tok.span,
                    help: None,
                }.into()),
            _ => (),
        }
        let expr = self.parse_expr()?;
        let body = self.parse_compound()?;
        let kind = match self.tok.kind {
            TokenKind::Kwd(KwdKind::Else) => {
                self.advance()?;
                match self.tok.kind {
                    TokenKind::Kwd(KwdKind::If) =>
                        // 'else if'
                        CondKind::If(
                            Box::new(expr),
                            Box::new(body),
                            Some(Box::new(self.parse_if()?)),
                        ),
                    TokenKind::BraceOpen(BraceKind::Curly) => {
                        // 'else' (no expr)
                        let else_body = self.parse_compound()?;
                        let else_ast = Expr::new(ExprKind::Cond(
                            CondKind::Else(Box::new(else_body))
                        ));
                        CondKind::If(
                            Box::new(expr),
                            Box::new(body),
                            Some(Box::new(else_ast)),
                        )
                    },
                    _ =>
                        return Err(CortexError::SyntaxError {
                            file_path: self.ctx.file_path(),
                            msg: format!("expected else body but got '{}'", self.tok.value()),
                            span: self.tok.span,
                            help: None
                        }.into())
                }
            },
            _ => CondKind::If(Box::new(expr), Box::new(body), None),
        };
        Ok(Expr::new(ExprKind::Cond(kind)))
    }

    /// Parses a type annotation, e.g., `x: i32`.
    fn parse_type_annotation(&mut self, with_ident_ctx: IdentCtx) -> Result<Ident> {
        let ident = self.expect_id(format!("expected identifier but got '{}'", self.tok.value()))?;
        self.eat(TokenKind::Delim(DelimKind::Colon))?;
        if let TokenKind::Ty(ty_kind) = self.tok.kind.clone() {
            self.advance()?; // skip type
            Ok(Ident::new(&ident, ty_kind, with_ident_ctx))
        } else {
            Err(CortexError::SyntaxError {
                file_path: self.ctx.file_path(),
                msg: format!("expected type but got '{}'", self.tok.value()),
                span: self.tok.span,
                help: None,
            }.into())
        }
    }

    /// Parses a return statement.
    fn parse_ret(&mut self) -> Result<Expr> {
        self.advance()?; // skip 'ret' kwd
        let expr = self.parse_expr()?;
        self.eat(TokenKind::Delim(DelimKind::Scolon))?;
        Ok(Expr::new(ExprKind::Stmt(StmtKind::Ret(Some(Box::new(expr))))))
    }

    /// Parses a let statement.
    fn parse_let(&mut self) -> Result<Expr> {
        self.advance()?; // skip 'let' kwd
        let ident = self.lexer.peek_token().and_then(|peek_tok| -> Result<Ident> {
            match peek_tok.kind {
                TokenKind::Delim(DelimKind::Colon) =>
                    self.parse_type_annotation(IdentCtx::Def),
                TokenKind::BinOp(BinOpKind::Eql) =>
                    Ok(Ident::new(
                        &self.expect_id(format!("expected identifier but got '{}'", self.tok.value()))?,
                        TyKind::Infer,
                        IdentCtx::Def,
                    )),
                _ =>
                    Err(CortexError::SyntaxError {
                        file_path: self.ctx.file_path(),
                        msg: format!("expected type annotation or equals sign but got '{}'", peek_tok.value()),
                        span: peek_tok.span,
                        help: None,
                    }.into())
            }
        })?;
        self.eat(TokenKind::BinOp(BinOpKind::Eql))?;
        // TODO: a bit hacky, but works for now. essentially, the left-hand side of the let
        // expression is parsed here instead of self.parse_expr() so the type annotation can be
        // captured properly. then, an expr is just returned anyways.
        let expr = Box::new(self.parse_expr()?);
        self.eat(TokenKind::Delim(DelimKind::Scolon))?;
        Ok(Expr::new(ExprKind::Stmt(StmtKind::Let(ident, expr))))
    }

    /// Parses a function.
    fn parse_func(&mut self) -> Result<Func> {
        self.advance()?; // skip 'func' kwd
        let func_id = self.expect_id(format!("expected function name but got '{}'", self.tok.value()))?;
        self.eat(TokenKind::BraceOpen(BraceKind::Paren))?;
        let mut params = Vec::new();
        if let TokenKind::Id(_) = self.tok.kind {
            loop {
                let param_ident = self.parse_type_annotation(IdentCtx::Param)?;
                params.push(param_ident);
                match &self.tok.kind {
                    TokenKind::Delim(DelimKind::Comma) => (),
                    _ => break,
                }
                self.advance()?;
            }
        }
        self.eat(TokenKind::BraceClosed(BraceKind::Paren))?;
        let ret_ty = match &self.tok.kind {
            TokenKind::Arrow => {
                self.advance()?;
                self.expect_ty()?
            },
            TokenKind::BraceOpen(BraceKind::Curly) => {
                TyKind::Void
            },
            _ =>
                return Err(CortexError::SyntaxError {
                    file_path: self.ctx.file_path(),
                    msg: format!("expected a return-type annotation or the beginning of a function body but got '{}'", self.tok.value()),
                    span: self.tok.span,
                    help: None,
                }.into())
        };
        let body = self.parse_compound()?;
        let node = Func::new(
            Ident::new(&func_id, ret_ty, IdentCtx::FuncDef),
            params,
            Box::new(body),
        );
        Ok(node)
    }

    /// Parses a term in an expression, i.e., a variable, literal, function call, etc..
    fn parse_term(&mut self) -> Result<Expr> {
        let node = match self.tok.kind.clone() {
            TokenKind::Num(n) => Expr::new(ExprKind::Lit(LitKind::Num(n))),
            TokenKind::BinOp(BinOpKind::Sub) => {
                // treat this as a unary operation
                self.advance()?; // skip operator
                return Ok(Expr::new(ExprKind::Unary(
                    UnaryOpKind::Neg,
                    Box::new(self.parse_term()?)
                )))
            }
            TokenKind::UnaryOp(op_kind) => {
                self.advance()?; // skip operator
                return Ok(Expr::new(ExprKind::Unary(
                    op_kind.clone(),
                    Box::new(self.parse_term()?)
                )));
            },
            TokenKind::Id(ref id) => 
                Expr::new(ExprKind::Id(
                    Ident::new(
                        id,
                        TyKind::Lookup,
                        IdentCtx::Ref,
                    )
                )),
            TokenKind::BraceOpen(ref brace_kind) if *brace_kind == BraceKind::Paren => {
                // pass opening parenthesis
                self.advance()?;
                let expr = self.parse_expr_helper(0)?;
                // expect closing parenthesis
                self.eat(TokenKind::BraceClosed(BraceKind::Paren))?;
                return Ok(expr);
            },
            _ =>
                return Err(CortexError::SyntaxError {
                    file_path: self.ctx.file_path(),
                    msg: format!("expected operand but got '{}'", self.tok.value()),
                    span: self.tok.span,
                    help: None,
                }.into())
        };
        self.advance()?;
        Ok(node)
    }

    /// The wrapper around [`Parser::parse_expr_helper`]. This method serves as a driver for the
    /// aforementioned function by providing a starting minimum precedence of zero.
    fn parse_expr(&mut self) -> Result<Expr> {
        let expr = self.parse_expr_helper(0)?;
        Ok(expr)
    }

    /// Parses a binary expression, e.g., `(8 - 4) * 4 - 3`.
    fn parse_expr_helper(&mut self, min_prec: i32) -> Result<Expr> {
        let mut lhs = self.parse_term()?;

        loop {
            match BinOpKind::maybe_from(&self.tok.kind) {
                Some(op_kind) => {
                    let prec = op_kind.prec();
                    if prec < min_prec {
                        break;
                    }
                    let assoc = op_kind.assoc();
                    let next_min_prec = match assoc {
                        OpAssoc::Right => prec + 1,
                        OpAssoc::Left => prec,
                    };
                    self.advance()?;
                    let rhs = self.parse_expr_helper(next_min_prec)?;
                    lhs = Expr::new(ExprKind::Binary(
                        op_kind,
                        Box::new(lhs),
                        Box::new(rhs),
                    ));
                },
                None => break,
            }
        }
        Ok(lhs)
    }

    /// Requests a token from the lexer, moving through the source file.
    fn advance(&mut self) -> Result {
        self.prev_tok = self.tok.clone();
        self.tok = self.lexer.next_token()?;
        Ok(())
    }

    /// Expects an identifier and returns an error if none is found.
    fn expect_id(&mut self, with_msg: String) -> Result<String> {
        let result = match &self.tok.kind {
            TokenKind::Id(ident) => Ok(ident.to_owned()),
            _ => Err(CortexError::SyntaxError {
                file_path: self.ctx.file_path(),
                msg: with_msg,
                span: self.tok.span,
                help: None,
            }.into())
        };
        self.advance()?;
        result
    }

    /// Expects to see a type and returns an error if none is found.
    fn expect_ty(&mut self) -> Result<TyKind> {
        let result = match &self.tok.kind {
            TokenKind::Ty(ty_kind) => Ok(ty_kind.to_owned()),
            _ => Err(CortexError::SyntaxError {
                file_path: self.ctx.file_path(),
                msg: format!("expected type but got '{}' instead", self.tok.value()),
                span: self.tok.span,
                help: None,
            }.into())
        };
        self.advance()?;
        result
    }

    /// Advances the parser by expecting a certain kind of token and returning an error if the next
    /// token does not match the expected one.
    fn eat(&mut self, expected_kind: TokenKind) -> Result {
        let tok_expected = match (&self.tok.kind, &expected_kind) {
            (TokenKind::Num(_), TokenKind::Num(_)) => true,
            (TokenKind::Id(_), TokenKind::Id(_)) => true,
            _ => self.tok.kind == expected_kind
        };
        if !tok_expected {
            return Err(CortexError::SyntaxError { 
                file_path: self.ctx.file_path(),
                msg: format!("expected '{}' but got '{}'", expected_kind.literal(), self.tok.value()),
                span: self.tok.span,
                help: None,
            }.into());
        }
        self.advance()?;
        Ok(())
    }
}
