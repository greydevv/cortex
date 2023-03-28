//! The main lexing interface.

use std::iter::Peekable;

use crate::io::file::{ FilePos, FileSpan };
use crate::io::error::{ CortexError, Result };
use crate::sess::SessCtx;
use crate::symbols::{
    Token,
    TokenKind,
    TyKind,
    LitKind,
    DelimKind,
    BraceKind,
    BinOpKind,
    UnaryOpKind,
    KwdKind,
    Len,
    MaybeFrom,
};

/// The lexer object.
pub struct Lexer<'a> {
    /// The current character.
    c: char,
    /// The context of the current compilation session.
    ctx: &'a SessCtx,
    /// The current position in the file.
    pos: FilePos,
    /// The stack keeping track of open/close braces.
    brace_stack: Vec<Token>,
    /// The source code as a peekable iterator.
    chars: Peekable<std::str::Chars<'a>>,
}

impl<'a> Lexer<'_> {
    /// Creates a new lexer from a given context.
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

    /// Advances the lexer through the source code.
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

    /// Peeks the next character from the source code and returns EOF if there are no characters
    /// left.
    fn peek_char(&mut self) -> char {
        self.chars.peek()
            .and_then(|c| Some(*c))
            .unwrap_or_else(|| '\0')
    }

    /// Generates a token from source code.
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

    /// Peeks the next token from the source code.
    pub fn peek_token(&mut self) -> Result<Token> {
        // TODO: possibly save peeked Token in an internal, private member such that a call to
        // next_token() after a peek_token() only must retrieve token from member save state. This
        // is a premature optimization and is unnecessary for now.
        let prev_pos = self.pos;
        let prev_c = self.c;
        let prev_iter = self.chars.clone();
        // get next token
        let res = self.next_token();
        // restore state
        self.chars = prev_iter;
        self.c = prev_c;
        self.pos = prev_pos;
        res
    }

    /// Generates a token from a lexeme beginning with an alphabetic character.
    ///
    /// # Examples
    /// These tokens are typically things like variables, keywords, types, etc.
    /// ```
    /// let x: i32 = multiply(2, 4);
    /// ```
    ///
    /// This method would handle the following lexemes:
    /// - `let` into a keyword
    /// - `x` into a variable identifier
    /// - `i32` into a type
    /// - `multiply` into a function identifier
    fn lex_alpha(&mut self) -> Token {
        let mut val = String::new();
        let beg_pos = self.pos;
        while self.c.is_alphanumeric() || self.c == '_' {
            val.push(self.c);
            self.next_char();
        }

        let span = FileSpan::new(beg_pos, self.pos);

        // attempt to coerce the lexeme to built-in literals, keywords, and types
        if let Some(bool_val) = match val.as_str() {
                "true" => Some(true),
                "false" => Some(false),
                _ => None,
        } {
            return Token::new(TokenKind::Lit(LitKind::Bool(bool_val)), span);
        } else if let Some(kwd_kind) = KwdKind::maybe_from(&val) {
            return Token::new(TokenKind::Kwd(kwd_kind), span);
        } else if let Some(ty_kind) = TyKind::maybe_from(&val) {
            return Token::new(TokenKind::Ty(ty_kind), span);
        } else {
            return Token::new(TokenKind::Id(val), span);
        }
    }

    /// Generates a token representing a numeric literal.
    ///
    /// It is worth noting that the parser is responsible for understanding the following two
    /// expressions are semantically valid and equivalent:
    ///
    /// ```
    /// 5 + -3
    /// 5 -3
    /// ```
    ///
    /// Furthermore, the lexer will not yield tokens representing negative numeric literals.
    /// The negation operation is a unary operation.
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
            Ok(n) => Ok(Token::new(TokenKind::Lit(LitKind::Num(n)), span)),
            Err(_) => Err(CortexError::invalid_integer_literal(self.ctx, &val, span).into())
        }
    }

    /// Generates a token from a static lexeme (i.e., delimiters, operators, etc.).
    fn lex_other(&mut self) -> Result<Token> {
        let beg_pos = self.pos;
        let kind = match self.c {
            '.' => TokenKind::Delim(DelimKind::Period),
            ',' => TokenKind::Delim(DelimKind::Comma),
            ';' => TokenKind::Delim(DelimKind::Scolon),
            ':' => TokenKind::Delim(DelimKind::Colon),
            '+' => TokenKind::BinOp(BinOpKind::Add),
            '-' => 
                match self.peek_char() {
                    '>' => TokenKind::Arrow,
                    _ => TokenKind::BinOp(BinOpKind::Sub),
                },
            '=' =>
                match self.peek_char() {
                    '=' => TokenKind::BinOp(BinOpKind::EqlBool),
                    _ => TokenKind::BinOp(BinOpKind::Eql),
                },
            '>' =>
                match self.peek_char() {
                    '=' => TokenKind::BinOp(BinOpKind::GrEql),
                    _ => TokenKind::BinOp(BinOpKind::Gr)
                },
            '<' =>
                match self.peek_char() {
                    '=' => TokenKind::BinOp(BinOpKind::LtEql),
                    _ => TokenKind::BinOp(BinOpKind::Lt)
                }
            '!' => TokenKind::UnaryOp(UnaryOpKind::Not),
            '*' => TokenKind::BinOp(BinOpKind::Mul),
            '/' => TokenKind::BinOp(BinOpKind::Div),
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

    /// Updates the brace balancing state.
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

    /// Generates a token representing a string literal.
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
                        TokenKind::Lit(LitKind::Str(val)),
                        span,
                    ));
                },
                _ => val.push(self.c)
            }
            self.next_char();
        }
    }

    /// Pass over meaningless characters, i.e., whitespace and comments.
    fn skip_junk(&mut self) {
        self.skip_whitespace();
        if self.c == '/' && self.peek_char() == '/'
        {
            self.skip_comment();
            self.skip_junk();
        }
    }

    /// Helper method used to skip comments.
    fn skip_comment(&mut self) {
        // skip entire line
        while self.c != '\n' && self.c != '\0' {
            self.next_char();
        }
    }

    /// Helper method used to skip whitespace characters.
    fn skip_whitespace(&mut self) {
        while self.c.is_whitespace() {
            self.next_char();
        }
    }

    /// Determines whether or not the lexer is at the end of the file (EOF).
    pub fn eof(&self) -> bool {
        self.c == '\0'
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::io::error::{
        Result,
        Diagnostic,
        DiagnosticKind,
    };
    use crate::io::file::FileHandler;
    use crate::io::error::tests::assert_diags;

    fn expect_toks_from_src(src: &str, expected_toks: Vec<Token>) -> Result {
        let ctx = SessCtx::new(FileHandler::pseudo_fh(src));
        let mut lexer = Lexer::new(&ctx);
        for expected in expected_toks {
            let tok = lexer.next_token()?;
            assert_eq!(tok.kind, expected.kind);
            assert_eq!(tok.span, expected.span);
        }
        Ok(())
    }

    fn expect_diags_from_src<D, R, T>(src: &str, get_diags: D, produce_result: R)
    where
        D: FnOnce(&mut Lexer) -> Vec<Diagnostic>,
        // Don't care what the wrapped type in Ok is.
        R: FnOnce(&mut Lexer) -> Result<T>,
    {
        let ctx = SessCtx::new(FileHandler::pseudo_fh(src));
        let mut lexer = Lexer::new(&ctx);
        let expected_diags = get_diags(&mut lexer);
        let result = produce_result(&mut lexer);

        assert_diags(result, expected_diags);
    }

    #[test]
    fn empty_file() -> Result {
        expect_toks_from_src(
            "",
            vec![
                Token::new(TokenKind::EOF, FileSpan::one(FilePos::new(1, 1)))
            ]
        )
    }

    #[test]
    fn leading_whitespace() -> Result {
        expect_toks_from_src(
            "  \n\n",
            vec![
                Token::new(TokenKind::EOF, FileSpan::one(FilePos::new(3, 1)))
            ]
        )
    }

    #[test]
    fn trailing_whitespace() -> Result {
        expect_toks_from_src(
            ";\n\n  ",
            vec![
                Token::new(TokenKind::Delim(DelimKind::Scolon), FileSpan::one(FilePos::new(1, 1))),
                Token::new(TokenKind::EOF, FileSpan::one(FilePos::new(3, 3)))
            ]
        )
    }

    #[test]
    fn leading_and_trailing_whitespace() -> Result {
        expect_toks_from_src(
            "  \n\n  \n  ",
            vec![
                Token::new(TokenKind::EOF, FileSpan::one(FilePos::new(4, 3)))
            ]
        )
    }

    #[test]
    fn keywords() -> Result {
        expect_toks_from_src(
            "func include for",
            vec![
                Token::new(TokenKind::Kwd(KwdKind::Func), FileSpan::new(FilePos::new(1, 1), FilePos::new(1, 5))),
                Token::new(TokenKind::Kwd(KwdKind::Include), FileSpan::new(FilePos::new(1, 6), FilePos::new(1, 13))),
                Token::new(TokenKind::Kwd(KwdKind::For), FileSpan::new(FilePos::new(1, 14), FilePos::new(1, 17))),
                Token::new(TokenKind::EOF, FileSpan::one(FilePos::new(1, 17))),
            ]
        )
    }

    #[test]
    fn boolean_literal() -> Result {
        expect_toks_from_src(
            "true false",
            vec![
                Token::new(TokenKind::Lit(LitKind::Bool(true)), FileSpan::new(FilePos::new(1, 1), FilePos::new(1, 5))),
                Token::new(TokenKind::Lit(LitKind::Bool(false)), FileSpan::new(FilePos::new(1, 6), FilePos::new(1, 11))),
            ]
        )
    }

    #[test]
    fn unterminated_string_literal() {
        // Lexer will encounter EOF before a closing double-quote
        expect_diags_from_src(
            "\"Hello, world!",
            |lexer| {
                vec![
                    Diagnostic::new_with_spans(
                        "unterminated string literal".to_string(),
                        DiagnosticKind::Error,
                        &lexer.ctx.fh,
                        vec![
                            (lexer.ctx.file_path(), FileSpan::new(FilePos::new(1, 1), FilePos::new(1, 15))),
                        ],
                    )
                ]
            },
            |lexer: &mut Lexer| {
                lexer.next_token()
            }
        )
    }

    #[test]
    fn closed_braces() -> Result {
        expect_toks_from_src(
            "([\n\n\n()]) [] {\n{{[{}]}\n}\n}",
            vec![
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
            ]
        )
    }

    #[test]
    fn unclosed_brace() {
        expect_diags_from_src(
            "(\n{)",
            |lexer| {
                vec![
                    Diagnostic::new_with_spans(
                        "unclosed '{'".to_string(),
                        DiagnosticKind::Error,
                        &lexer.ctx.fh,
                        vec![
                            (lexer.ctx.file_path(), FileSpan::one(FilePos::new(2, 1))),
                        ],
                    )
                ]
            },
            |lexer: &mut Lexer| {
                for _ in 0..2 {
                    assert!(lexer.next_token().is_ok());
                }
                lexer.next_token()
            }
        )
    }

    #[test]
    fn unopened_brace() {
        expect_diags_from_src(
            "(\n})",
            |lexer| {
                vec![
                    Diagnostic::new_with_spans(
                        "unopened '}'".to_string(),
                        DiagnosticKind::Error,
                        &lexer.ctx.fh,
                        vec![
                            (lexer.ctx.file_path(), FileSpan::one(FilePos::new(2, 1))),
                        ],
                    )
                ]
            },
            |lexer: &mut Lexer| {
                assert!(lexer.next_token().is_ok());
                lexer.next_token()
            }
        )
    }

    #[test]
    fn unary_negation() -> Result {
        expect_toks_from_src(
            "-13",
            vec![
                // Treat unary operator as a binary operator in lexer, leave it up to the parser to
                // determine if it is meant as a unary operator or a binary operator.
                Token::new(TokenKind::BinOp(BinOpKind::Sub), FileSpan::one(FilePos::new(1, 1))),
                Token::new(TokenKind::Lit(LitKind::Num(13)), FileSpan::new(FilePos::new(1, 2), FilePos::new(1, 4))),
            ]
        )
    }

    #[test]
    fn unary_not() -> Result {
        expect_toks_from_src(
            "!x",
            vec![
                Token::new(TokenKind::UnaryOp(UnaryOpKind::Not), FileSpan::one(FilePos::new(1, 1))),
                Token::new(TokenKind::Id(String::from("x")), FileSpan::one(FilePos::new(1, 2))),
            ]
        )
    }
}
