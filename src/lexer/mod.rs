use std::iter::Peekable;

use crate::io::file::{ FilePos, FileSpan };
use crate::io::error::{ CortexError, Result };
use crate::lexer::token::{
    Token,
    TokenKind,
    DelimKind,
    BraceKind,
    OpKind,
    TyKind,
    KwdKind,
    Len,
    MaybeFrom,
};
use crate::sess::SessCtx;

pub mod token;

pub struct Lexer<'a> {
    c: char,
    ctx: &'a SessCtx,
    pos: FilePos,
    brace_stack: Vec<Token>,
    chars: Peekable<std::str::Chars<'a>>,
}

impl<'a> Lexer<'_> {
    pub fn new(ctx: &'a SessCtx) -> Lexer<'a> {
        let mut chars = ctx.fh.contents().chars().peekable();
        let c = match chars.next() {
            Some(c) => c,
            None => '\0'
        };
        Lexer {
            c,
            ctx,
            brace_stack: Vec::new(),
            pos: FilePos::default(),
            chars,
        }
    }

    fn next_char(&mut self) {
        if self.c == '\0' {
            return;
        }
        match self.c {
            '\0' => (),
            '\n' => {
                self.pos.line += 1;
                self.pos.col = 1;
            },
            _ => self.pos.col += 1,
        }
        self.c = match self.chars.next() {
            Some(c) => c,
            None => '\0',
        };

    }

    fn peek_char(&mut self) -> char {
        match self.chars.peek() {
            Some(c) => *c,
            None => '\0',
        }
    }

    pub fn next_token(&mut self) -> Result<Token> {
        self.skip_junk();
        if self.eof() {
            // brace balancing state (brace_stack) should be empty if all opened braces were
            // closed at some point
            match self.brace_stack.last() {
                Some(unclosed_brace) => Err(CortexError::unclosed_brace(self.ctx, &unclosed_brace).into()),
                None => Ok(Token::eof(self.pos))
            }
        } else if self.c.is_alphabetic() {
            Ok(self.lex_alpha())
        } else if self.c.is_numeric() {
            self.lex_num()
        } else if self.c == '"' {
            self.lex_string()
        } else {
            self.lex_other()
        }
    }

    fn lex_alpha(&mut self) -> Token {
        let mut val = String::new();
        let beg_pos = self.pos;
        while self.c.is_alphanumeric() || self.c == '_' {
            val.push(self.c);
            self.next_char();
        }

        // return a keyword token if the string was found to be a built-in keyword
        let span = FileSpan::new(beg_pos, self.pos);
        if let Some(kwd_kind) = KwdKind::maybe_from(&val) {
            return Token::new(TokenKind::Kwd(kwd_kind), span);
        } else if let Some(ty_kind) = TyKind::maybe_from(&val) {
            return Token::new(TokenKind::Ty(ty_kind), span);
        } else {
            return Token::new(TokenKind::Id(val), span);
        }
    }

    /// Returns a Token containing data representing a numeric literal.
    ///
    /// It is worth noting that the Parser is responsible for understanding the following two
    /// expressions are semantically valid and equivalent:
    ///
    /// ```
    /// 5 + -3
    /// 5 -3
    /// ```
    ///
    /// The Lexer yields three tokens (Num, Op, Num) for the first expression and only two (Num,
    /// Num) for the second equation. In the case of the second expression, the Parser must expand
    /// the negative numeric literal into a subtraction operation followed by a positive numeric
    /// literal, like so:
    ///
    /// ```
    /// 5 -3 // (5) - (3)
    /// ```
    ///
    /// # Examples
    ///
    /// ```
    /// "13"  // Token(TokenKind::Num, "13", ...)
    ///
    /// "-13" // Token(TokenKind::Num, "-13", ...)
    /// ```
    ///
    fn lex_num(&mut self) -> Result<Token> {
        // initializing with current char before the loop allows a negative sign to appear before a
        // numeric literal
        let mut val = String::from(self.c);
        let beg_pos = self.pos;
        self.next_char();
        while self.c.is_alphanumeric() {
            val.push(self.c);
            self.next_char();
        }
        let span = FileSpan::new(beg_pos, self.pos);
        match val.parse::<i32>() {
            Ok(n) => Ok(Token::new(TokenKind::Num(n), span)),
            Err(_) => Err(CortexError::invalid_integer_literal(self.ctx, &val, span).into())
        }
    }

    fn lex_other(&mut self) -> Result<Token> {
        let beg_pos = self.pos;
        let kind = match self.c {
            '.' => TokenKind::Delim(DelimKind::Period),
            ',' => TokenKind::Delim(DelimKind::Comma),
            ';' => TokenKind::Delim(DelimKind::Scolon),
            ':' => TokenKind::Delim(DelimKind::Colon),
            '+' => TokenKind::Op(OpKind::Add),
            '-' => 
                match self.peek_char() {
                    '>' => TokenKind::Arrow,
                    c if c.is_numeric() => return self.lex_num(),
                    _ => TokenKind::Op(OpKind::Sub),
                },
            '=' =>
                match self.peek_char() {
                    '=' => TokenKind::Op(OpKind::EqlBool),
                    _ => TokenKind::Op(OpKind::Eql),
                },
            '>' =>
                match self.peek_char() {
                    '=' => TokenKind::Op(OpKind::GrEql),
                    _ => TokenKind::Op(OpKind::Gr)
                },
            '<' =>
                match self.peek_char() {
                    '=' => TokenKind::Op(OpKind::LtEql),
                    _ => TokenKind::Op(OpKind::Lt)
                }
            '*' => TokenKind::Op(OpKind::Mul),
            '/' => TokenKind::Op(OpKind::Div),
            '(' => TokenKind::BraceOpen(BraceKind::Paren),
            ')' => TokenKind::BraceClosed(BraceKind::Paren),
            '{' => TokenKind::BraceOpen(BraceKind::Curly),
            '}' => TokenKind::BraceClosed(BraceKind::Curly),
            '[' => TokenKind::BraceOpen(BraceKind::Square),
            ']' => TokenKind::BraceClosed(BraceKind::Square),
            _ => TokenKind::Unknown(self.c),
        };
        for _ in 0..kind.len() {
            self.next_char();
        }

        let span = FileSpan::new(beg_pos, self.pos);

        let tok = Token::new(kind, span);
        match tok.kind {
            TokenKind::BraceOpen(_) | TokenKind::BraceClosed(_) => self.update_balancing_state(tok.clone())?,
            _ => ()
        }
        Ok(tok)
    }

    fn update_balancing_state(&mut self, tok: Token) -> Result {
        match &tok.kind {
            TokenKind::BraceOpen(_) => self.brace_stack.push(tok),
            TokenKind::BraceClosed(_) => {
                let top_tok = match self.brace_stack.pop() {
                    // proceed normally if the current closing brace matches an opening
                    // brace on top of stack
                    Some(opening_tok) if tok.closes(&opening_tok) => return Ok(()),
                    Some(opening_tok) => opening_tok,
                    // stack is empty, the current closing brace is unopened
                    None => return Err(CortexError::unopened_brace(self.ctx, &tok).into()),
                };
                // unwind the balancing state (brace_stack) to see if the current closing token
                // was opened somewhere previously
                while let Some(opening_tok) = self.brace_stack.pop() {
                    // found a matching opening token, the token on the top of the stack was
                    // unclosed
                    if tok.closes(&opening_tok) {
                        return Err(CortexError::unclosed_brace(self.ctx, &top_tok).into());
                    }
                }
                // did not find a matching opening token, the current closing token is unopened
                return Err(CortexError::unopened_brace(self.ctx, &tok).into());
            },
            _ => ()
        }
        Ok(())
    }

    fn lex_string(&mut self) -> Result<Token> {
        let mut val = String::new();
        let beg_pos = self.pos;

        // eat opening quote
        self.next_char();

        loop {
            match self.c {
                '\0' => {
                    return Err(CortexError::syntax_err(
                        self.ctx,
                        "unterminated string literal",
                        FileSpan::new(beg_pos, self.pos),
                        None,
                    ).into());
                },
                '"' => {
                    // eat closing quote
                    self.next_char();
                    let span = FileSpan::new(beg_pos, self.pos);
                    return Ok(Token::new(
                        TokenKind::String(val),
                        span,
                    ));
                },
                _ => val.push(self.c)
            }
            self.next_char();
        }
    }

    fn skip_junk(&mut self) {
        self.skip_whitespace();
        if self.c == '/' && *self.chars.peek().unwrap() == '/'
        {
            self.skip_comment();
            self.skip_junk();
        }
    }

    fn skip_comment(&mut self) {
        // skip entire line
        while self.c != '\n' && self.c != '\0' {
            self.next_char();
        }
    }

    fn skip_whitespace(&mut self) {
        while self.c.is_whitespace() {
            self.next_char();
        }
    }

    pub fn eof(&self) -> bool {
        self.c == '\0'
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_file() -> Result {
        let src = String::new();
        let mut lexer = Lexer::new(&src);

        let tok = lexer.next_token()?;
        assert_eq!(tok.kind, TokenKind::EOF);
        assert_eq!(tok.span, FileSpan::one(FilePos::new(1, 1)));
        Ok(())
    }

    #[test]
    fn leading_whitespace() -> Result {
        let src = String::from("  \n\n");
        let mut lexer = Lexer::new(&src);

        let tok = lexer.next_token()?;
        assert_eq!(tok.kind, TokenKind::EOF);
        assert_eq!(tok.span, FileSpan::one(FilePos::new(3, 1)));
        Ok(())
    }

    #[test]
    fn trailing_whitespace() -> Result {
        let src = String::from(";\n\n  ");
        let mut lexer = Lexer::new(&src);

        lexer.next_token()?;
        let tok = lexer.next_token()?;
        assert_eq!(tok.kind, TokenKind::EOF);
        assert_eq!(tok.span, FileSpan::one(FilePos::new(3, 3)));
        Ok(())
    }

    #[test]
    fn leading_and_trailing_whitespace() -> Result {
        let src = String::from("  \n\n  \n  ");
        let mut lexer = Lexer::new(&src);

        let tok = lexer.next_token()?;
        assert_eq!(tok.kind, TokenKind::EOF);
        assert_eq!(tok.span, FileSpan::one(FilePos::new(4, 3)));
        Ok(())
    }

    #[test]
    fn keywords() -> Result {
        let src = String::from("func include for");
        let mut lexer = Lexer::new(&src);
        let expected_toks = vec![
            Token::new(TokenKind::Kwd(KwdKind::Func), FileSpan::new(FilePos::new(1, 1), FilePos::new(1, 5))),
            Token::new(TokenKind::Kwd(KwdKind::Include), FileSpan::new(FilePos::new(1, 6), FilePos::new(1, 13))),
            Token::new(TokenKind::Kwd(KwdKind::For), FileSpan::new(FilePos::new(1, 14), FilePos::new(1, 17))),
            Token::new(TokenKind::EOF, FileSpan::one(FilePos::new(1, 17))),
        ];

        for expected in expected_toks {
            let tok = lexer.next_token()?;
            assert_eq!(tok.kind, expected.kind);
            assert_eq!(tok.span, expected.span);
        }
        Ok(())
    }

    #[test]
    fn unterminated_string_literal() {
        // Lexer will encounter EOF before a closing double-quote
        let src = String::from("\"Hello, world!");
        let mut lexer = Lexer::new(&src);
        let result = lexer.next_token();
        let expected = CortexError::syntax_err("unterminated string literal", FileSpan::new(FilePos::new(1, 1), FilePos::new(1, 15)));

        assert!(result.is_err());
        assert_eq!(result.err().unwrap(), expected);
    }

    #[test]
    fn closed_braces() -> Result {
        let src = String::from("([\n\n\n()]) [] {\n{{[{}]}\n}\n}");
        let expected_toks = vec![
            Token::new(TokenKind::BraceOpen(BraceKind::Paren), FileSpan::one(FilePos::new(1, 1))),
            Token::new(TokenKind::BraceOpen(BraceKind::Square), FileSpan::one(FilePos::new(1, 2))),
            Token::new(TokenKind::BraceOpen(BraceKind::Paren), FileSpan::one(FilePos::new(4, 1))),
            Token::new(TokenKind::BraceClosed(BraceKind::Paren), FileSpan::one(FilePos::new(4, 2))),
            Token::new(TokenKind::BraceClosed(BraceKind::Square), FileSpan::one(FilePos::new(4, 3))),
            Token::new(TokenKind::BraceClosed(BraceKind::Paren), FileSpan::one(FilePos::new(4, 4))),
            Token::new(TokenKind::BraceOpen(BraceKind::Square), FileSpan::one(FilePos::new(4, 6))),
            Token::new(TokenKind::BraceClosed(BraceKind::Square), FileSpan::one(FilePos::new(4, 7))),
            Token::new(TokenKind::BraceOpen(BraceKind::Curly), FileSpan::one(FilePos::new(4, 9))),
            Token::new(TokenKind::BraceOpen(BraceKind::Curly), FileSpan::one(FilePos::new(5, 1))),
            Token::new(TokenKind::BraceOpen(BraceKind::Curly), FileSpan::one(FilePos::new(5, 2))),
            Token::new(TokenKind::BraceOpen(BraceKind::Square), FileSpan::one(FilePos::new(5, 3))),
            Token::new(TokenKind::BraceOpen(BraceKind::Curly), FileSpan::one(FilePos::new(5, 4))),
            Token::new(TokenKind::BraceClosed(BraceKind::Curly), FileSpan::one(FilePos::new(5, 5))),
            Token::new(TokenKind::BraceClosed(BraceKind::Square), FileSpan::one(FilePos::new(5, 6))),
            Token::new(TokenKind::BraceClosed(BraceKind::Curly), FileSpan::one(FilePos::new(5, 7))),
            Token::new(TokenKind::BraceClosed(BraceKind::Curly), FileSpan::one(FilePos::new(6, 1))),
            Token::new(TokenKind::BraceClosed(BraceKind::Curly), FileSpan::one(FilePos::new(7, 1))),
            Token::eof(FilePos::new(7,2)),
        ];
        let mut lexer = Lexer::new(&src);

        for expected in expected_toks {
            let tok = lexer.next_token()?;
            assert_eq!(tok.kind, expected.kind);
            assert_eq!(tok.span, expected.span);
        }
        Ok(())
    }

    #[test]
    fn unclosed_brace() {
        let src = String::from("(\n{)");
        let mut lexer = Lexer::new(&src);

        for _ in 0..2 {
            assert!(lexer.next_token().is_ok());
        }

        let expected = CortexError::syntax_err("unclosed '{'", FileSpan::one(FilePos::new(2, 1)));
        let result = lexer.next_token();

        assert!(result.is_err());
        assert_eq!(result.err().unwrap(), expected);
    }

    #[test]
    fn unopened_brace() {
        let src = String::from("(\n})");
        let mut lexer = Lexer::new(&src);

        // no need to loop, first next_token is only call that should succeed
        assert!(lexer.next_token().is_ok());

        let expected = CortexError::syntax_err("unopened '}'", FileSpan::one(FilePos::new(2, 1)));
        let result = lexer.next_token();

        assert!(result.is_err());
        assert_eq!(result.err().unwrap(), expected);
    }
}
