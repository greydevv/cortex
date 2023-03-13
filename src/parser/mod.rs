use crate::io::error::{ Result, CortexError };
use crate::lexer::Lexer;
use crate::ast::{ AstNode, IdentCtx, AstConditionalKind };
use crate::lexer::token::{
    Token,
    TokenKind,
    BinOpKind,
    OpAssoc,
    TyKind,
    KwdKind,
    DelimKind,
    BraceKind,
    Literal,
    MaybeFrom,
};
use crate::sess::SessCtx;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    ctx: &'a SessCtx,
    tok: Token,
    prev_tok: Token,
}

impl<'a> Parser<'_> {
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

    pub fn parse(&mut self) -> Result<Vec<Box<AstNode>>> {
        let mut tree = Vec::new();
        while !self.tok.is_eof() {
            if let TokenKind::Kwd(kwd_kind) = &self.tok.kind {
                let child = match kwd_kind.clone() {
                    KwdKind::Include => self.parse_include()?,
                    KwdKind::Func => self.parse_func()?,
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

    fn parse_compound(&mut self) -> Result<AstNode> {
        let mut children = Vec::new();
        self.eat(TokenKind::BraceOpen(BraceKind::Curly))?;
        loop {
            let child = match self.tok.kind.clone() {
                TokenKind::Id(_) | TokenKind::Num(_) => {
                    let expr = self.parse_expr()?;
                    self.eat(TokenKind::Delim(DelimKind::Scolon))?;
                    expr
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
        Ok(AstNode::Compound(children))
    }

    fn parse_include(&mut self) -> Result<AstNode> {
        self.advance()?; // skip 'include' kwd
        if let TokenKind::String(_) = self.tok.kind {
            let incl = AstNode::Include(self.tok.value());
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

    fn parse_kwd(&mut self, kwd_kind: &KwdKind) -> Result<AstNode> {
        let node = match *kwd_kind {
            KwdKind::Include => self.parse_include()?,
            KwdKind::Func => self.parse_func()?,
            KwdKind::Let => self.parse_let()?,
            KwdKind::Ret => {
                self.advance()?; // skip 'ret' kwd
                let expr = self.parse_expr()?;
                self.eat(TokenKind::Delim(DelimKind::Scolon))?;
                AstNode::Stmt {
                    kind: kwd_kind.clone(),
                    expr: Box::new(expr),
                }
            },
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
            _ => unimplemented!("parse_kwd: '{}'", *kwd_kind),
        };
        Ok(node)
    }

    fn parse_while(&mut self) -> Result<AstNode> {
        self.advance()?; // skip 'while' kwd
        let expr = self.parse_expr()?;
        let body = self.parse_compound()?;
        let kind = AstConditionalKind::While { expr: Box::new(expr) };
        Ok(AstNode::Cond {
            kind,
            body: Box::new(body),
        })
    }

    fn parse_if(&mut self) -> Result<AstNode> {
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
                    TokenKind::Kwd(KwdKind::If) => {
                        AstConditionalKind::If {
                            expr: Box::new(expr),
                            other: Box::new(self.parse_if()?)
                        }
                    },
                    TokenKind::BraceOpen(BraceKind::Curly) => {
                        let else_body = self.parse_compound()?;
                        let else_ast = AstNode::Cond {
                            kind: AstConditionalKind::Else,
                            body: Box::new(else_body),
                        };
                        AstConditionalKind::If {
                            expr: Box::new(expr),
                            other: Box::new(else_ast),
                        }
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
            _ => AstConditionalKind::SoloIf { expr: Box::new(expr) },
        };
        Ok(AstNode::Cond {
            kind,
            body: Box::new(body),
        })
    }

    fn parse_type_annotation(&mut self, ident_ctx: IdentCtx) -> Result<AstNode> {
        let id = self.expect_id(format!("expected identifier but got '{}'", self.tok.value()))?;
        self.eat(TokenKind::Delim(DelimKind::Colon))?;
        if let TokenKind::Ty(ty_kind) = self.tok.kind.clone() {
            self.advance()?; // skip type
            Ok(AstNode::Id(id, ty_kind, ident_ctx))
        } else {
            Err(CortexError::SyntaxError {
                file_path: self.ctx.file_path(),
                msg: format!("expected type but got '{}'", self.tok.value()),
                span: self.tok.span,
                help: None,
            }.into())
        }
    }

    fn parse_let(&mut self) -> Result<AstNode> {
        self.advance()?; // skip 'let' kwd
        let lhs = self.lexer.peek_token().and_then(|peek_tok| -> Result<AstNode> {
            match peek_tok.kind {
                TokenKind::Delim(DelimKind::Colon) => self.parse_type_annotation(IdentCtx::Decl),
                TokenKind::BinOp(BinOpKind::Eql) =>
                    Ok(AstNode::Id(
                        self.expect_id(format!("expected identifier but got '{}'", self.tok.value()))?,
                        TyKind::Infer,
                        IdentCtx::Decl,
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
        let expr = AstNode::BinExpr {
            op: BinOpKind::Eql,
            lhs: Box::new(lhs),
            rhs: Box::new(self.parse_expr()?),
        };
        self.eat(TokenKind::Delim(DelimKind::Scolon))?;
        return Ok(AstNode::Stmt {
            kind: KwdKind::Let,
            expr: Box::new(expr),
        })
    }

    fn parse_func(&mut self) -> Result<AstNode> {
        self.advance()?; // skip 'func' kwd
        let func_id = self.expect_id(format!("expected function name but got '{}'", self.tok.value()))?;
        self.eat(TokenKind::BraceOpen(BraceKind::Paren))?;
        let mut params = Vec::new();
        if let TokenKind::Id(_) = self.tok.kind {
            loop {
                let param_id = self.parse_type_annotation(IdentCtx::Param)?;
                params.push(Box::new(param_id));
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
        let node = AstNode::Func {
            id: func_id,
            ret_ty,
            params,
            body: Box::new(body)
        };
        Ok(node)
    }

    fn parse_term(&mut self) -> Result<AstNode> {
        let node = match self.tok.kind.clone() {
            TokenKind::Num(n) => AstNode::Num(n),
            TokenKind::UnaryOp(op_kind) => {
                self.advance()?;
                return Ok(AstNode::UnaryExpr {
                    op: op_kind.clone(),
                    rhs: Box::new(self.parse_term()?),
                });
            },
            TokenKind::Id(ref id) => AstNode::Id(id.clone(), TyKind::Lookup, IdentCtx::Ref),
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

    fn parse_expr(&mut self) -> Result<AstNode> {
        let expr = self.parse_expr_helper(0)?;
        Ok(expr)
    }

    fn parse_expr_helper(&mut self, min_prec: i32) -> Result<AstNode> {
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
                    lhs = AstNode::BinExpr {
                        op: op_kind,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }
                },
                None => break,
            }
        }
        Ok(lhs)
    }

    fn advance(&mut self) -> Result {
        self.prev_tok = self.tok.clone();
        self.tok = self.lexer.next_token()?;
        Ok(())
    }

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
