use crate::io::file::FileHandler;
use crate::io::error::CortexError;
use crate::lexer::Lexer;
use crate::ast::AstNode;
use crate::lexer::token::{ Token, TokenKind, OpKind, OpAssoc, DelimKind, BraceKind, BraceFace, Literal };

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    tok: Token,
}

impl<'a> Parser<'_> {
    pub fn new(file_handler: &'a FileHandler) -> Result<Parser<'a>, CortexError> {
        let mut lexer = Lexer::new(file_handler.contents());
        let tok = lexer.next_token()?;
        Ok(Parser {
            lexer,
            tok,
        })
    }

    pub fn parse(&mut self) -> Result<Vec<AstNode>, CortexError> {
        let mut tree = Vec::new();
        while !self.tok.is_eof() {
            let child = match self.tok.kind {
                TokenKind::Id(_) => self.parse_expr()?,
                TokenKind::Kwd(ref kwd_kind) => {
                    let kind = kwd_kind.clone();
                    self.advance()?;
                    AstNode::Stmt {
                        kind,
                        expr: Box::new(self.parse_expr()?)
                    }
                },
                _ => unimplemented!("How to start?"),
            };
            tree.push(child);
        }
        Ok(tree)
    }

    fn parse_term(&mut self) -> Result<AstNode, CortexError> {
        let node = match &self.tok.kind {
            TokenKind::Num(n) => AstNode::Num(*n),
            TokenKind::Id(id) => AstNode::Id(id.clone()),
            TokenKind::Brace(brace_kind, brace_face) if *brace_kind == BraceKind::Paren && *brace_face == BraceFace::Open => {
                // pass opening parenthesis
                self.advance()?;
                let expr = self.parse_expr_helper(0)?;
                // expect closing parenthesis
                self.eat(TokenKind::Brace(BraceKind::Paren, BraceFace::Closed))?;
                return Ok(expr);
            },
            _ => return Err(CortexError::SyntaxError(format!("unknown operand in binary expression: {}", self.tok), self.tok.loc, None))
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
                None => return Err(CortexError::expected_bin_op(&self.tok.kind.literal(), self.tok.loc)),
            }
        }
        Ok(lhs)
    }

    fn advance(&mut self) -> Result<(), CortexError> {
        self.tok = self.lexer.next_token()?;
        Ok(())
    }

    fn eat(&mut self, expected_kind: TokenKind) -> Result<(), CortexError> {
        let tok_expected = match (&self.tok.kind, &expected_kind) {
            (TokenKind::Num(_), TokenKind::Num(_)) => true,
            _ => self.tok.kind == expected_kind
        };
        if !tok_expected {
            return Err(CortexError::SyntaxError(format!("expected '{}' but got '{}'", expected_kind.literal(), self.tok.kind.literal()), self.tok.loc, None));
        }
        self.advance()?;
        Ok(())
    }
}
