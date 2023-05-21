//! The main parsing interface.

use std::ops::Deref;

use crate::io::error::{
    Result,
    CortexError,
    Diagnostic,
    DiagnosticKind
};
use crate::io::file::{ FilePath, FileHandler, FileSpan, SourceLoc };
use crate::lexer::Lexer;
use crate::ast::{
    Compound,
    Module,
    Stmt,
    Func,
    Enum,
    Expr,
    Ident,
    IdentInfo,
    ExprKind,
    StmtKind,
    IdentCtx,
};
use crate::symbols::{
    Token,
    TokenKind,
    BinOpKind,
    LitKind,
    UnaryOpKind,
    OpAssoc,
    TyKind,
    KwdKind,
    DelimKind,
    BraceKind,
    Literal,
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
    pub fn parse(&mut self) -> Result<Module> {
        let mut module = Module::new(self.ctx.file_path().deref().clone());
        while !self.tok.is_eof() {
            if let TokenKind::Kwd(kwd_kind) = &self.tok.kind {
                let child = match kwd_kind.clone() {
                    KwdKind::Include => self.parse_include()?,
                    KwdKind::Func => self.parse_func()?,
                    KwdKind::Enum => self.parse_enum()?,
                    _ => unimplemented!("error message for bad start keyword: '{}'", kwd_kind)
                };
                module.add_node(child);
            } else {
                return Err(CortexError::expected_but_got(
                    &self.ctx,
                    "keyword",
                    &self.tok,
                ).into());
            }
        }
        Ok(module)
    }

    /// Parses an enumeration definition.
    fn parse_enum(&mut self) -> Result<Stmt> {
        // Skip 'enum' keyword.
        let enum_kwd_span = self.tok.span;
        self.advance()?;
        let ident_span = self.tok.span;
        let ident_string = self.expect_id("identifier")?;
        let mut enumer = Enum::new(
            Ident::new_unqual(
                IdentInfo::new(
                    ident_string.clone(),
                    TyKind::UserDef(ident_string),
                    IdentCtx::EnumDef,
                    self.new_loc(ident_span),
                )
            )
        );
        self.eat(TokenKind::BraceOpen(BraceKind::Curly))?;
        self.parse_enum_members(&mut enumer)?;
        let closing_span = self.tok.span;
        self.eat(TokenKind::BraceClosed(BraceKind::Curly))?;
        Ok(Stmt::new(StmtKind::Enum(enumer), self.new_loc(enum_kwd_span.to(&closing_span))))
    }

    fn parse_enum_members(&mut self, enumer: &mut Enum) -> Result {
        loop {
            if let TokenKind::BraceClosed(BraceKind::Curly) = self.tok.kind {
                // self.advance()?; // skip closing parenthesis
                break;
            }
            let ident_span = self.tok.span;
            let ident = Ident::new_unqual(
                IdentInfo::new(
                    self.expect_id("identifier")?,
                    enumer.ident().ty_kind().clone(),
                    IdentCtx::Def,
                    self.new_loc(ident_span),
                )
            );
            if let Some(original_ident) = enumer.get_member(&ident) {
                return Err(CortexError::dupe_enum_member(
                    &self.ctx,
                    &ident,
                    original_ident,
                ).into())
            }
            enumer.add_member(ident);
            if let TokenKind::Delim(DelimKind::Comma) = self.tok.kind {
                self.advance()?;
            }
        }

        Ok(())
    }

    /// Parses a compound expression, i.e., a collection of statements and/or expressions inside a
    /// set of curly braces, `{` and `}`.
    fn parse_compound(&mut self) -> Result<Compound> {
        let mut compound = Compound::new();
        // This will be reset in the loop
        let mut span = self.tok.span;
        self.eat(TokenKind::BraceOpen(BraceKind::Curly))?;
        loop {
            let child = match self.tok.kind.clone() {
                TokenKind::Id(_) | TokenKind::Lit(_) => {
                    let expr = self.parse_expr()?;
                    let expr_loc = expr.loc().clone();
                    self.eat(TokenKind::Delim(DelimKind::Scolon))?;
                    Stmt::new(StmtKind::Expr(expr), expr_loc)
                },
                TokenKind::Kwd(ref kwd_kind) => {
                    // Remember the index at which the compound was broken out of, e.g., return,
                    // break, etc.
                    if *kwd_kind == KwdKind::Ret && compound.get_break_idx().is_none() {
                        compound.set_break_idx(compound.stmts().len());
                    }
                    self.parse_kwd(kwd_kind)?
                },
                TokenKind::BraceClosed(BraceKind::Curly) => {
                    span = span.to(&self.tok.span);
                    self.advance()?;
                    break;
                }
                _ => unimplemented!("DEV ERROR: NOT SURE HOW TO PROCEED ON TOKEN: {}", self.tok),
            };
            compound.add_stmt(child);
        }
        compound.set_span(span);
        Ok(compound)
    }

    /// Parses an include statement, e.g., `include "some_file.cx"`.
    fn parse_include(&mut self) -> Result<Stmt> {
        let incl_kwd_span = self.tok.span;
        self.advance()?; // skip 'include' kwd
        if let TokenKind::Lit(LitKind::Str(_)) = self.tok.kind {
            let file_path = FilePath::new(self.tok.value().as_str());
            let module = self.resolve_include(file_path)?;
            let incl = Stmt::new(StmtKind::Incl(module), self.new_loc(incl_kwd_span.to(&self.tok.span)));
            self.advance()?; // skip string
            self.eat(TokenKind::Delim(DelimKind::Scolon))?;
            Ok(incl)
        } else {
            Err(CortexError::expected_but_got(
                &self.ctx,
                "string literal",
                &self.tok
            ).into())
        }
    }

    /// Resolves an include statement and returns its AST.
    fn resolve_include(&mut self, file_path: FilePath) -> Result<Module> {
        let ctx = SessCtx::new(FileHandler::new(file_path)?);
        let mut parser = Parser::new(&ctx)?;
        parser.parse()
    }

    /// Continues parsing according to the keyword token the parser has encountered.
    ///
    /// # Examples
    /// If the parser encounters the `func` keyword, it will continue by attempting to parse a
    /// function.
    ///
    /// If the parser encounters the `if` keyword, it will continue by attempting to parse an
    /// if/else if/else statement.
    fn parse_kwd(&mut self, kwd_kind: &KwdKind) -> Result<Stmt> {
        let node = match *kwd_kind {
            // early return for function
            KwdKind::Func => self.parse_func()?,
            KwdKind::Include => self.parse_include()?,
            KwdKind::Let => self.parse_let()?,
            KwdKind::Ret => self.parse_ret()?,
            KwdKind::If => self.parse_if()?,
            KwdKind::Else => {
                self.advance()?;
                let (kind, span) = match self.tok.kind {
                    TokenKind::Kwd(KwdKind::If) => ("else if", self.prev_tok.span.to(&self.tok.span)),
                    _ => ("else", self.prev_tok.span),
                };

                return Err(CortexError(
                    vec![
                        Diagnostic::new_with_spans(
                            &self.ctx,
                            format!("found '{}' without a preceding 'if' statement", kind),
                            DiagnosticKind::Error,
                            vec![self.new_loc(span)],
                        )
                    ]
                ).into())
            }
            KwdKind::While => self.parse_while()?,
            KwdKind::For => unimplemented!("parsing of for loop"),
            KwdKind::Enum => self.parse_enum()?,
        };
        Ok(node)
    }

    /// Parses a while loop.
    fn parse_while(&mut self) -> Result<Stmt> {
        let while_kwd_span = self.tok.span;
        self.advance()?; // skip 'while' kwd
        let expr = match self.tok.kind {
            TokenKind::BraceOpen(BraceKind::Curly) => None,
            _ => Some(self.parse_expr()?),
        };
        let body = self.parse_compound()?;
        let span = while_kwd_span.to(&body.span());
        Ok(Stmt::new(StmtKind::While(expr, body), self.new_loc(span)))
    }

    /// Parses an if/else if/else statement.
    fn parse_if(&mut self) -> Result<Stmt> {
        let if_kwd_span = self.tok.span;
        self.advance()?; // skip 'if' kwd
        match self.tok.kind {
            TokenKind::BraceOpen(BraceKind::Curly) =>
                return Err(CortexError::expected_but_got(self.ctx, "expression", &self.tok).into()),
            _ => (),
        }
        let expr = self.parse_expr()?;
        let body = self.parse_compound()?;
        let kind = match self.tok.kind {
            TokenKind::Kwd(KwdKind::Else) => {
                let else_kwd_span = self.tok.span;
                self.advance()?;
                match self.tok.kind {
                    TokenKind::Kwd(KwdKind::If) =>
                        // 'else if'
                        StmtKind::If(
                            expr,
                            body,
                            Some(Box::new(self.parse_if()?)),
                        ),
                    TokenKind::BraceOpen(BraceKind::Curly) => {
                        // 'else' (no expr)
                        let else_body = self.parse_compound()?;
                        let span = else_kwd_span.to(&else_body.span());
                        let else_ast = Stmt::new(StmtKind::Else(else_body), self.new_loc(span));
                        StmtKind::If(
                            expr,
                            body,
                            Some(Box::new(else_ast)),
                        )
                    },
                    _ =>
                        return Err(CortexError::expected_but_got(
                            &self.ctx,
                            "else body",
                            &self.tok,
                        ).into()),
                }
            },
            _ => StmtKind::If(expr, body, None),
        };
        Ok(Stmt::new(kind, self.new_loc(if_kwd_span.to(&self.prev_tok.span))))
    }

    /// Parses a type annotation, e.g., `x: i32`.
    fn parse_type_annotation(&mut self, with_ident_ctx: IdentCtx) -> Result<Ident> {
        let ident_span = self.tok.span;
        let ident = self.expect_id("identifier")?;
        self.eat(TokenKind::Delim(DelimKind::Colon))?;
        if let TokenKind::Ty(ty_kind) = self.tok.kind.clone() {
            self.advance()?; // skip type
            Ok(Ident::new_unqual(
                IdentInfo::new(
                    ident,
                    ty_kind,
                    with_ident_ctx,
                    self.new_loc(ident_span))
                )
            )
        } else {
            Err(CortexError::expected_but_got(
                &self.ctx,
                "type",
                &self.tok,
            ).into())
        }
    }

    /// Parses a return statement.
    fn parse_ret(&mut self) -> Result<Stmt> {
        let ret_kwd_span = self.tok.span;
        self.advance()?; // skip 'ret' kwd
        let (expr, span) = if let TokenKind::Delim(DelimKind::Scolon) = self.tok.kind {
            (None, ret_kwd_span)
        } else {
            let expr = self.parse_expr()?;
            let span = ret_kwd_span.to(expr.loc().span());
            (Some(expr), span)
        };
        self.eat(TokenKind::Delim(DelimKind::Scolon))?;
        Ok(Stmt::new(StmtKind::Ret(expr), self.new_loc(span)))
    }

    /// Parses a let statement.
    fn parse_let(&mut self) -> Result<Stmt> {
        let let_kwd_span = self.tok.span;
        self.advance()?; // skip 'let' kwd
        let ident = if let TokenKind::Id(_) = self.tok.kind {
            self.lexer.peek_token().and_then(|peek_tok| -> Result<Ident> {
                match peek_tok.kind {
                    TokenKind::Delim(DelimKind::Colon) =>
                        self.parse_type_annotation(IdentCtx::Def),
                    TokenKind::BinOp(BinOpKind::Eql) => {
                        let ident_span = self.tok.span;
                        Ok(Ident::new_unqual(
                            IdentInfo::new(
                                self.expect_id("identifier")?,
                                TyKind::Infer,
                                IdentCtx::Def,
                                self.new_loc(ident_span)),
                            )
                        )
                    },
                    _ =>
                        Err(CortexError::expected_but_got(
                            &self.ctx,
                            "type annotation or equals sign",
                            &peek_tok,
                        ).into()),
                }
            })
        } else {
            Err(CortexError::expected_but_got(
                &self.ctx,
                "identifier",
                &self.tok,
            ).into())
        }?;
        self.eat(TokenKind::BinOp(BinOpKind::Eql))?;
        // A bit hacky, but works for now. essentially, the left-hand side of the let
        // expression is parsed here instead of self.parse_expr() so the type annotation can be
        // captured properly. Then, an expr is just returned anyways.
        let expr = self.parse_expr()?;
        let span = let_kwd_span.to(expr.loc().span());
        self.eat(TokenKind::Delim(DelimKind::Scolon))?;
        Ok(Stmt::new(StmtKind::Let(ident, expr), self.new_loc(span)))
    }

    /// Parses a function.
    fn parse_func(&mut self) -> Result<Stmt> {
        let func_kwd_span = self.tok.span;
        self.advance()?; // skip 'func' kwd
        let func_ident_span = self.tok.span;
        let func_ident = self.expect_id("function name")?;
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
                return Err(CortexError::expected_but_got(
                    &self.ctx,
                    "return-type annotation or function body",
                    &self.tok,
                ).into()),
        };
        let body = self.parse_compound()?;
        let body_span = body.span().clone();
        let func = Func::new(
            Ident::new_unqual(
                IdentInfo::new(
                    func_ident,
                    ret_ty,
                    IdentCtx::FuncDef,
                    self.new_loc(func_ident_span)
                )
            ),
            params,
            body,
        );
        Ok(Stmt::new(StmtKind::Func(func), self.new_loc(func_kwd_span.to(&body_span))))
    }

    /// Parses a term in an expression, i.e., a variable, literal, function call, etc..
    fn parse_term(&mut self) -> Result<Expr> {
        let node = match self.tok.kind.clone() {
            TokenKind::Lit(lit_kind) => Expr::new(ExprKind::Lit(lit_kind), self.new_loc(self.tok.span)),
            TokenKind::BinOp(BinOpKind::Sub) => {
                // treat this as a unary operation
                let op_span = self.tok.span;
                self.advance()?; // skip operator
                let expr = self.parse_term()?;
                let span = op_span.to(expr.loc().span());
                return Ok(Expr::new(ExprKind::Unary(
                    UnaryOpKind::Neg,
                    Box::new(expr)
                ), self.new_loc(span)));
            }
            TokenKind::UnaryOp(op_kind) => {
                let op_span = self.tok.span;
                self.advance()?; // skip operator
                let expr = self.parse_term()?;
                let span = op_span.to(expr.loc().span());
                return Ok(Expr::new(ExprKind::Unary(
                    op_kind.clone(),
                    Box::new(expr)
                ), self.new_loc(span)));
            },
            TokenKind::Id(ref id) => {
                return self.parse_ident_or_call(id)
            }
            TokenKind::BraceOpen(ref brace_kind) if *brace_kind == BraceKind::Paren => {
                // pass opening parenthesis
                self.advance()?;
                let expr = self.parse_expr_helper(0)?;
                // expect closing parenthesis
                self.eat(TokenKind::BraceClosed(BraceKind::Paren))?;
                return Ok(expr);
            },
            _ =>
                return Err(CortexError::expected_but_got(
                    &self.ctx,
                    "operand",
                    &self.tok,
                ).into()),
        };
        self.advance()?;
        Ok(node)
    }

    fn parse_ident(&mut self, first_ident: &String) -> Result<Ident> {
        let mut span = self.tok.span;
        let mut raw_ident = first_ident.clone();
        let mut qualifier = None;
        // skip id token
        self.advance()?;
        if self.tok.kind == TokenKind::Delim(DelimKind::ScopeSep) {
            // skip scope separator ('::')
            self.advance()?;
            qualifier = Some(IdentInfo::new(
                raw_ident,
                TyKind::Lookup,
                IdentCtx::Ref,
                self.new_loc(span),
            ));
            // Get next span and identifier.
            span = self.tok.span;
            raw_ident = self.expect_id("identifier")?;
        }
        let info = IdentInfo::new(
            raw_ident,
            TyKind::Lookup,
            IdentCtx::Ref,
            self.new_loc(span),
        );
        // loop {
        //     if self.tok.kind == TokenKind::Delim(DelimKind::ScopeSep) {
        //         // skip scope separator ('::')
        //         self.advance()?;
        //         qualifier.push(Ident::new(
        //             &raw_ident,
        //             TyKind::Lookup,
        //             IdentCtx::Ref,
        //             self.new_loc(self.tok.span))
        //         );
        //         span = span.to(&self.tok.span);
        //         raw_ident = self.expect_id("identfier")?;
        //     } else {
        //         break;
        //     }
        // }
        let ident = match qualifier {
            Some(qual_info) => Ident::new_qual(qual_info, info),
            None => Ident::new_unqual(info),
        };
        Ok(ident)
    }

    /// Parses a basic identifier or a function call if the identifier is followed by opening
    /// parenthesis.
    fn parse_ident_or_call(&mut self, first_ident: &String) -> Result<Expr> {
        let mut expr_span = self.tok.span;
        let mut ident = self.parse_ident(first_ident)?;
        let expr_kind = match self.tok.kind {
            TokenKind::BraceOpen(BraceKind::Paren) => {
                let open_paren_span = self.tok.span;
                self.advance()?; // skip opening parenthesis
                let args = self.parse_comma_sep_expr()?;
                let close_paren_span = self.tok.span;
                self.eat(TokenKind::BraceClosed(BraceKind::Paren))?;
                expr_span = open_paren_span.to(&close_paren_span);
                ident.set_ctx(IdentCtx::FuncCall);
                ExprKind::Call(ident, args)
            }
            _ => 
                ExprKind::Id(ident)
        };

        Ok(Expr::new(expr_kind, self.new_loc(expr_span)))
    }

    /// Parses a comma separated list of expressions, such as expressions passed as arguments to
    /// function calls.
    fn parse_comma_sep_expr(&mut self) -> Result<Vec<Box<Expr>>> {
        let mut exprs = Vec::new();
        loop {
            if let TokenKind::BraceClosed(BraceKind::Paren) = self.tok.kind {
                // self.advance()?; // skip closing parenthesis
                break;
            }
            exprs.push(Box::new(self.parse_expr()?));
            if let TokenKind::Delim(DelimKind::Comma) = self.tok.kind {
                self.advance()?;
            }
        }

        Ok(exprs)
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
            match self.tok.kind.clone() {
                TokenKind::BinOp(bin_op_kind) => {
                    let prec = bin_op_kind.prec();
                    if prec < min_prec {
                        break;
                    }
                    let assoc = bin_op_kind.assoc();
                    let next_min_prec = match assoc {
                        OpAssoc::Right => prec + 1,
                        OpAssoc::Left => prec,
                    };
                    self.advance()?;
                    let rhs = self.parse_expr_helper(next_min_prec)?;
                    let span = lhs.loc().span().to(rhs.loc().span());
                    lhs = Expr::new(ExprKind::Binary(
                        bin_op_kind,
                        Box::new(lhs),
                        Box::new(rhs),
                    ), self.new_loc(span));
                },
                _ => break,
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
    fn expect_id(&mut self, with_msg: &str) -> Result<String> {
        let result = match &self.tok.kind {
            TokenKind::Id(ident) => Ok(ident.to_owned()),
            _ => Err(CortexError::expected_but_got(self.ctx, with_msg, &self.tok).into())
        };
        self.advance()?;
        result
    }

    /// Expects to see a type and returns an error if none is found.
    fn expect_ty(&mut self) -> Result<TyKind> {
        let result = match &self.tok.kind {
            TokenKind::Ty(ty_kind) => Ok(ty_kind.to_owned()),
            _ => 
                Err(CortexError::expected_but_got(
                    &self.ctx,
                    "type",
                    &self.tok,
                ).into()),
        };
        self.advance()?;
        result
    }

    /// Advances the parser by expecting a certain kind of token and returning an error if the next
    /// token does not match the expected one.
    fn eat(&mut self, expected_kind: TokenKind) -> Result {
        let tok_expected = match (&self.tok.kind, &expected_kind) {
            _ => self.tok.kind == expected_kind
        };
        if !tok_expected {
            return Err(CortexError::expected_but_got(
                &self.ctx,
                format!("'{}'", expected_kind.literal()).as_str(),
                &self.tok,
            ).into());
        }
        self.advance()?;
        Ok(())
    }

    /// Creates a new instance of a source location from the file currently being parsed.
    fn new_loc(&self, span: FileSpan) -> SourceLoc {
        SourceLoc::new(self.ctx.file_path(), span)
    }
}
