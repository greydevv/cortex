use crate::io::file::FileHandler;
use crate::io::error::CortexError;
use crate::lexer::Lexer;
use crate::ast::AstNode;
use crate::lexer::token::{ Token, TokenKind, OpKind, OpAssoc, TyKind, KwdKind, DelimKind, BraceKind, Literal };

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    tok: Token,
    prev_tok: Token,
}

impl<'a> Parser<'_> {
    pub fn new(file_handler: &'a FileHandler) -> Result<Parser<'a>, CortexError> {
        let mut lexer = Lexer::new(file_handler.contents());
        let tok = lexer.next_token()?;
        Ok(Parser {
            lexer,
            tok,
            prev_tok: Token::dummy(),
        })
    }

    pub fn parse(&mut self) -> Result<Vec<Box<AstNode>>, CortexError> {
        let mut tree = Vec::new();
        while !self.tok.is_eof() {
            let child = match self.tok.kind.clone() {
                TokenKind::Id(_) => self.parse_expr()?,
                TokenKind::Kwd(ref kwd_kind) => self.parse_kwd(kwd_kind)?,
                TokenKind::BraceOpen(_) | TokenKind::BraceClosed(_) => break,
                _ => unimplemented!("How to start?: {}", self.tok),
            };
            tree.push(Box::new(child));
        }
        Ok(tree)
    }

    fn parse_kwd(&mut self, kwd_kind: &KwdKind) -> Result<AstNode, CortexError> {
        let node = match *kwd_kind {
            KwdKind::Func => self.parse_func()?,
            KwdKind::Let | KwdKind::Ret => {
                self.advance()?; // skip kwd
                AstNode::Stmt {
                    kind: kwd_kind.clone(),
                    expr: Box::new(self.parse_expr()?)
                }
            },
            _ => unimplemented!("parse_kwd: '{}'", *kwd_kind),
        };
        Ok(node)
    }

    fn parse_func(&mut self) -> Result<AstNode, CortexError> {
        self.advance()?; // skip 'func' kwd
        let func_id = self.expect_id(format!("expected function name but found '{}' instead", self.tok.value()))?;
        self.eat(TokenKind::BraceOpen(BraceKind::Paren))?;
        let mut params = Vec::new();
        if let TokenKind::Id(_) = self.tok.kind {
            loop {
                let param_id = self.expect_id(format!("expected parameter name after comma, but found '{}' instead", self.tok.value()))?;
                params.push(Box::new(AstNode::Id(param_id)));
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
            _ => return Err(CortexError::SyntaxError(format!("expected a return-type annotation or the beginning of a function body but got '{}'", self.tok.value()), self.tok.span, None))
        };
        self.eat(TokenKind::BraceOpen(BraceKind::Curly))?;
        let body = self.parse()?;
        self.eat(TokenKind::BraceClosed(BraceKind::Curly))?;
        let node = AstNode::Func {
            id: func_id,
            ret_ty,
            params,
            body: Box::new(AstNode::Compound { children: body })
        };
        Ok(node)
    }

    fn parse_term(&mut self) -> Result<AstNode, CortexError> {
        let node = match &self.tok.kind {
            TokenKind::Num(n) => AstNode::Num(*n),
            TokenKind::Id(ref id) => AstNode::Id(id.clone()),
            TokenKind::BraceOpen(ref brace_kind) if *brace_kind == BraceKind::Paren => {
                // pass opening parenthesis
                self.advance()?;
                let expr = self.parse_expr_helper(0)?;
                // expect closing parenthesis
                self.eat(TokenKind::BraceClosed(BraceKind::Paren))?;
                return Ok(expr);
            },
            _ => return Err(CortexError::SyntaxError(format!("expected operand after binary operator but got '{}'", self.tok.value()), self.tok.span, None))
        };
        self.advance()?;
        Ok(node)
    }

    fn parse_expr(&mut self) -> Result<AstNode, CortexError> {
        let expr = self.parse_expr_helper(0)?;
        self.eat(TokenKind::Delim(DelimKind::Scolon))?;
        Ok(expr)
    }

    fn parse_expr_helper(&mut self, min_prec: i32) -> Result<AstNode, CortexError> {
        let mut lhs = self.parse_term()?;

        loop {
            match self.tok.kind {
                TokenKind::Op(_) => (),
                TokenKind::EOF | _ => break,
            }
            match OpKind::from(&self.tok.kind) {
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
                    lhs = AstNode::BinaryExpr {
                        op: op_kind,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }
                },
                None => return Err(CortexError::expected_bin_op(&self.tok.value(), self.tok.span)),
            }
        }
        Ok(lhs)
    }

    fn advance(&mut self) -> Result<(), CortexError> {
        self.prev_tok = self.tok.clone();
        self.tok = self.lexer.next_token()?;
        Ok(())
    }

    fn expect_id(&mut self, with_msg: String) -> Result<String, CortexError> {
        let result = match &self.tok.kind {
            TokenKind::Id(ident) => Ok(ident.to_owned()),
            _ => Err(CortexError::SyntaxError(with_msg, self.tok.span, None))
        };
        self.advance()?;
        result
    }

    fn expect_ty(&mut self) -> Result<TyKind, CortexError> {
        let result = match &self.tok.kind {
            TokenKind::Ty(ty_kind) => Ok(ty_kind.to_owned()),
            _ => Err(CortexError::SyntaxError(format!("expected type but got '{}' instead", self.tok.value()), self.tok.span, None))
        };
        self.advance()?;
        result
    }

    fn eat(&mut self, expected_kind: TokenKind) -> Result<(), CortexError> {
        let tok_expected = match (&self.tok.kind, &expected_kind) {
            (TokenKind::Num(_), TokenKind::Num(_)) => true,
            (TokenKind::Id(_), TokenKind::Id(_)) => true,
            _ => self.tok.kind == expected_kind
        };
        if !tok_expected {
            return Err(CortexError::SyntaxError(format!("expected '{}' but got '{}'", expected_kind.literal(), self.tok.value()), self.tok.span, None));
        }
        self.advance()?;
        Ok(())
    }
}
